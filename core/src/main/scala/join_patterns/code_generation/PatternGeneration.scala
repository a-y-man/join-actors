package join_patterns.code_generation

import join_actors.actor.ActorRef
import join_patterns.types.{*, given}
import join_patterns.util.*

import scala.collection.immutable.{TreeMap as MTree}
import scala.quoted.{Expr, Quotes, Type}

/** Builds extractor tuples (type name, type checker, field extractor, guard filter)
  * for each constructor in a pattern. Used by both unary and composite pattern generation.
  *
  * @param typesData
  *   constructor types and their field bindings.
  * @param filters
  *   per-type filtering lambdas from guard generation.
  * @return
  *   a list of 4-tuples for each constructor in the pattern.
  */
private[code_generation] def buildExtractorTuples[M](using quotes: Quotes, tm: Type[M])(
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])],
    filters: Map[String, Expr[GuardFilter]]
): List[(Expr[String], Expr[M => Boolean], Expr[M => LookupEnv], Expr[GuardFilter])] =
  import quotes.reflect.*

  typesData.map { (outer, fields) =>
    val extractor = generateExtractor(outer, fields.map(_._1))

    outer.asType match
      case '[ot] =>
        val typeName = TypeTree.of[ot].symbol.name
        val filteringLambda = filters.getOrElse(typeName, '{ (_: LookupEnv) => true })

        (
          Expr(typeName),
          '{ (m: M) => m.isInstanceOf[ot] },
          '{ (m: M) =>
            ${ extractor.asExprOf[ot => LookupEnv] }(m.asInstanceOf[ot])
          },
          filteringLambda
        )
  }.toList

/** Generates a join pattern for one or more message constructors.
  * Handles both unary (single message) and composite (multiple message) patterns.
  *
  * For unary patterns, creates a single-key `PatternBins` and single-entry `PatternExtractors`.
  * For composite patterns, groups type names into multi-key bins and creates indexed extractors.
  *
  * @param patterns
  *   the pattern trees (one for unary, multiple for composite `&:&` patterns).
  * @param guard
  *   the optional guard predicate.
  * @param rhsTerm
  *   the right-hand side of the pattern.
  * @param selfSym
  *   the symbol of the self ActorRef parameter.
  * @return
  *   a join pattern expression.
  */

/** Extracts Bind symbols from pattern trees (excluding wildcards `_`). */
private[code_generation] def extractPatternBindSymbols(using quotes: Quotes)(
    patterns: List[quotes.reflect.Tree]
): List[(String, quotes.reflect.Symbol)] =
  import quotes.reflect.*

  val accumulator = new TreeAccumulator[List[(String, Symbol)]]:
    override def foldTree(acc: List[(String, Symbol)], tree: Tree)(owner: Symbol): List[(String, Symbol)] =
      tree match
        case b @ Bind(name, _) if name != "_" => (name, b.symbol) :: foldOverTree(acc, tree)(owner)
        case e                                => foldOverTree(acc, e)(owner)

  patterns.flatMap(p => accumulator.foldTree(Nil, p)(Symbol.spliceOwner))

/** Pairs each pattern variable with the symbol of its `Bind` and the type of its field.
  *
  * Guards and right-hand sides refer to pattern variables through these symbols, so a different
  * variable that has the same name (an inner `val`, `def`, lambda parameter, nested pattern
  * variable, or a definition in an enclosing scope) is never mistaken for a pattern variable.
  * Wildcards are not bindings and are left out.
  */
private[code_generation] def extractPatternBindings(using quotes: Quotes)(
    patterns: List[quotes.reflect.Tree],
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])]
): List[(String, quotes.reflect.Symbol, quotes.reflect.TypeRepr)] =
  import quotes.reflect.*

  val symbolOf = extractPatternBindSymbols(patterns).toMap
  typesData.flatMap(_._2).collect {
    case (name, tpe) if name != "_" =>
      val sym = symbolOf.getOrElse(
        name,
        report.errorAndAbort(s"Internal macro error: no binding symbol found for pattern variable '$name'")
      )
      (name, sym, tpe)
  }

/** Validates the variable bindings of a join pattern.
  *
  * Pattern variables are identified by symbol when guards and right-hand sides are rewritten, so
  * shadowing of a pattern variable by an inner or outer definition is harmless and not reported.
  * What is still rejected:
  *
  *   - a name bound more than once across the constructors of one join pattern. Bound values are
  *     passed to the guard and right-hand side in a map keyed by variable name, so the names of a
  *     join pattern must be unique. (Scala's own checker usually catches this already; this is
  *     defense-in-depth.)
  *   - a guard that refers to the actor's `self` reference. A guard is evaluated by the matcher
  *     on messages before the actor handles them, when there is no actor context to refer to.
  */
private[code_generation] def checkPatternBindings(using quotes: Quotes)(
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])],
    selfSym: quotes.reflect.Symbol,
    guard: Option[quotes.reflect.Term]
): Unit =
  import quotes.reflect.*

  var hasErrors = false

  // 1. Duplicate variable names across constructors in composite patterns.
  val allBindings = typesData.flatMap { (typeRepr, fields) =>
    fields.collect { case (name, _) if name != "_" => (name, typeRepr.typeSymbol.name) }
  }
  val duplicates = allBindings
    .groupBy(_._1)
    .collect { case (name, occurrences) if occurrences.size > 1 =>
      occurrences.map(_._2).mkString(", ")
    }
  for constructors <- duplicates do
    report.error(
      s"Variable name is bound multiple times across constructors ($constructors) in the same join pattern. " +
        s"Each binding must have a unique name."
    )
    hasErrors = true

  // 2. `self` used in a guard.
  val selfUses = new TreeAccumulator[List[Ident]]:
    override def foldTree(acc: List[Ident], tree: Tree)(owner: Symbol): List[Ident] =
      tree match
        case id: Ident if id.symbol == selfSym => id :: acc
        case e                                 => foldOverTree(acc, e)(owner)
  for term <- guard.toList; use <- selfUses.foldTree(Nil, term)(Symbol.spliceOwner) do
    report.error(
      s"The guard of a join pattern cannot refer to `${selfSym.name}`: guards are evaluated on " +
        s"messages before the actor handles them. Use `${selfSym.name}` in the body of the case instead.",
      use.pos
    )
    hasErrors = true

  if hasErrors then
    report.errorAndAbort("Join pattern has invalid variable bindings (see errors above).")

/** Verifies that each message constructor type in the pattern is a subtype of `M`.
  *
  * Since the `receive` macro accepts `PartialFunction[Any, ...]`, Scala does not
  * enforce that pattern constructors match the declared message type. This check
  * ensures at compile time that every `case Foo(...)` pattern uses a type `Foo <: M`,
  * preventing silent runtime mismatches where a constructor would never match.
  */
private[code_generation] def checkPatternSubtypesOfM[M](using quotes: Quotes, tm: Type[M])(
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])]
): Unit =
  import quotes.reflect.*

  val mType = TypeRepr.of[M].dealias.simplified

  for (constructorType, _) <- typesData do
    val ct = constructorType.dealias.simplified
    if !(ct <:< mType) then
      report.errorAndAbort(
        s"Message pattern type '${ct.show}' is not a subtype of the declared message type '${mType.show}'. " +
          s"All patterns in a join definition must match subtypes of the actor's message type."
      )

private[code_generation] def generateJP[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(
    patterns: List[quotes.reflect.Tree],
    guard: Option[quotes.reflect.Term],
    rhsTerm: quotes.reflect.Term,
    selfSym: quotes.reflect.Symbol
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = extractConstructorData(patterns)
  checkPatternSubtypesOfM[M](typesData)
  checkPatternBindings(typesData, selfSym, guard)
  val bindings = extractPatternBindings(patterns, typesData)
  val (predicate, filters) = generateGuard(guard, typesData, bindings)
  val extractors = buildExtractorTuples[M](typesData, filters)

  val size = typesData.size

  val patternInfo: Expr[PatternInfo[M]] =
    if size == 1 then
      '{
        val extractorList = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
        val checkMsgType = extractorList.head._2
        val extractField = extractorList.head._3
        val filterer = extractorList.head._4

        PatternInfo(
          patternBins = MTree(PatternIdxs(0) -> MessageIdxs()),
          patternExtractors =
            PatternExtractors(0 -> PatternIdxInfo(checkMsgType, extractField, filterer))
        )
      }
    else
      val patExtractors: Expr[PatternExtractors[M]] = '{
        val extractorList = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
        extractorList.zipWithIndex.map {
          case ((_, checkMsgType, extractField, filterer), idx) =>
            idx -> PatternIdxInfo(checkMsgType, extractField, filterer)
        }.toMap
      }

      '{
        val extractorList = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
        val msgTypesInPattern = extractorList.map(pat => (pat._1, pat._2)).zipWithIndex
        val patBins =
          msgTypesInPattern
            .groupBy(_._1._1)
            .map { case (checkMsgType, occurrences) =>
              val indices = occurrences.map(_._2)
              indices.iterator.to(PatternIdxs) -> MessageIdxs()
            }

        PatternInfo(patternBins = patBins.to(MTree), patternExtractors = $patExtractors)
      }

  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] =
    generateRhs[M, T](rhsTerm, bindings, selfSym).asExprOf[(LookupEnv, ActorRef[M]) => T]

  '{
    JoinPattern(
      $predicate,
      $rhs,
      ${ Expr(size) },
      ${ patternInfo }
    )
  }

/** Generates a join pattern for a wildcard pattern (`case _ => ...`).
  *
  * @param guard
  *   the optional guard predicate.
  * @param rhsTerm
  *   the right-hand side of the pattern.
  * @param selfSym
  *   the symbol of the self ActorRef parameter.
  * @return
  *   a join pattern expression with empty pattern bins and extractors.
  */
private[code_generation] def generateWildcardPattern[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(
    guard: Option[quotes.reflect.Term],
    rhsTerm: quotes.reflect.Term,
    selfSym: quotes.reflect.Symbol
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  checkPatternBindings(Nil, selfSym, guard)
  val (predicate, filter) = generateGuard(guard, Nil, Nil)
  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] =
    generateRhs[M, T](rhsTerm, Nil, selfSym).asExprOf[(LookupEnv, ActorRef[M]) => T]
  val size = 1

  val patternInfo: Expr[PatternInfo[M]] = '{
    PatternInfo(
      patternBins = MTree(),
      patternExtractors = Map()
    )
  }

  '{
    JoinPattern(
      $predicate,
      $rhs,
      ${ Expr(size) },
      ${ patternInfo }
    )
  }

/** Dispatches a `CaseDef` to the appropriate pattern generator based on its structure.
  *
  * Routes to:
  * - `generateJP` for single-message patterns (`case Msg(x) => ...`)
  * - `generateJP` for composite patterns (`case A(x) &:& B(y) => ...`)
  * - `generateWildcardPattern` for wildcard patterns (`case _ => ...`)
  *
  * @param joinPattern
  *   the case definition to process.
  * @param selfSym
  *   the symbol of the self ActorRef parameter.
  * @return
  *   an optional join pattern expression, or `None` if the pattern is unsupported.
  */
private[code_generation] def generateJoinPattern[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(
    joinPattern: quotes.reflect.CaseDef,
    selfSym: quotes.reflect.Symbol
): Option[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  joinPattern match
    case CaseDef(pattern, guard, rhsTerm) =>
      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, subPatterns), _) =>
          fun match
            case Select(_, "unapply") =>
              Some(generateJP[M, T](List(t), guard, rhsTerm, selfSym))
            case TypeApply(Select(_, "unapply"), _) =>
              errorTreeWithHint(
                "Unsupported message constructor type",
                "Extractors with type parameters, such as generic case classes, are not supported; " +
                  "use a message class without type parameters",
                fun
              )
              None
            case other =>
              errorTreeWithHint(
                "Unsupported extractor in a join pattern",
                "Only the pattern of a case class, like `MsgType(field1, field2)`, is supported " +
                  "(sequence patterns such as `Msg(xs*)` are not)",
                other
              )
              None
        case andOperatorApplication @ Unapply(_, _, _) =>
          val patterns = getConstructorPatternsFromAndOps[M, T](andOperatorApplication)
          Some(generateJP[M, T](patterns, guard, rhsTerm, selfSym))
        case w: Wildcard =>
          Some(generateWildcardPattern[M, T](guard, rhsTerm, selfSym))
        case default =>
          errorTreeWithHint(
            "Unsupported case pattern",
            "Patterns must be case class constructors (e.g., `case Msg(x)`) or composite patterns (e.g., `case A(x) &:& B(y)`)",
            default
          )
          None
