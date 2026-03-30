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
  * @param selfRefName
  *   the name of the self ActorRef parameter.
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

/** Checks whether `name` is declared in any enclosing scope of `bindSym`,
  * indicating that the pattern variable shadows an outer binding.
  */
private[code_generation] def isNameInEnclosingScope(using quotes: Quotes)(
    name: String,
    bindSym: quotes.reflect.Symbol
): Boolean =
  import quotes.reflect.*

  def check(sym: Symbol): Boolean =
    if sym.isNoSymbol || sym.isPackageDef then false
    else
      val found = sym.declarations.exists(d =>
        d.name == name && d != bindSym && !d.isNoSymbol
      )
      if found then true
      else check(sym.maybeOwner)

  // Start from the Bind symbol's owner (the enclosing scope that contains the CaseDef)
  check(bindSym.maybeOwner)

/** Finds inner bindings in a term whose names collide with pattern variable names.
  *
  * Only detects bindings that create genuinely new scopes:
  *   - Function/lambda parameters (`Flags.Param`): e.g. `list.map(x => x + 1)` where `x`
  *     is also a pattern variable — the `TreeMap` would incorrectly replace the lambda's `x`.
  *   - Nested match `Bind` nodes: e.g. `val match { case A(x) => ... }` inside the RHS.
  *
  * Regular `ValDef`s (including those from `inline` function expansion) are excluded
  * because the name-based `TreeMap` handles them correctly — the inlined alias is
  * initialised with the replaced pattern variable value.
  */
private[code_generation] def findInnerShadowingBindings(using quotes: Quotes)(
    term: quotes.reflect.Term,
    fieldBindingNames: Set[String]
): List[String] =
  import quotes.reflect.*

  val accumulator = new TreeAccumulator[List[String]]:
    override def foldTree(acc: List[String], tree: Tree)(owner: Symbol): List[String] =
      tree match
        case vd @ ValDef(name, _, _)
            if fieldBindingNames.contains(name) && vd.symbol.flags.is(Flags.Param) =>
          name :: foldOverTree(acc, tree)(owner)
        case Bind(name, _) if fieldBindingNames.contains(name) =>
          name :: foldOverTree(acc, tree)(owner)
        case _ =>
          foldOverTree(acc, tree)(owner)

  accumulator.foldTree(Nil, term)(Symbol.spliceOwner).distinct

/** Validates that join pattern bindings do not shadow variables from outer or inner scopes.
  *
  * The guard and RHS substitution (`replaceInnersWithLookupEnv`, `generateRhs`)
  * replaces `Ident` nodes by **name**, not by Symbol identity. This means:
  *
  *   - If a pattern binds `x` and an outer scope also defines `x`, the developer
  *     cannot reference the outer `x` in the guard/RHS — it will silently become
  *     a `LookupEnv` lookup for the pattern-bound value.
  *
  *   - If a pattern binds `x` and the guard/RHS contains an inner binding
  *     (lambda parameter, val, nested match) also named `x`, the `TreeMap`
  *     will incorrectly replace the inner `x` with a `LookupEnv` lookup.
  *
  * Both cases are reported as compile errors so the developer can rename variables.
  */
private[code_generation] def checkForShadowedBindings(using quotes: Quotes)(
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])],
    selfRefName: String,
    guard: Option[quotes.reflect.Term],
    rhsTerm: quotes.reflect.Term,
    patterns: List[quotes.reflect.Tree]
): Unit =
  import quotes.reflect.*

  val fieldBindingNames = typesData
    .flatMap(_._2)
    .map(_._1)
    .filter(_ != "_")
    .toSet

  var hasErrors = false

  // 1. Detect pattern bindings that shadow variables in enclosing scopes.
  //    This includes the self ActorRef parameter and any val/var/def visible
  //    at the receive call site.
  val bindSymbols = extractPatternBindSymbols(patterns)
  for (name, bindSym) <- bindSymbols do
    if isNameInEnclosingScope(name, bindSym) then
      report.error(
        s"Pattern variable '$name' shadows a binding with the same name in an enclosing scope. " +
          s"Use a different name to avoid ambiguity.",
        bindSym.pos.getOrElse(Position.ofMacroExpansion)
      )
      hasErrors = true

  // 2. Detect inner bindings in the guard/RHS that shadow pattern variables.
  //    These cause incorrect substitution because the TreeMap replaces by name.
  val termsToCheck = guard.toList :+ rhsTerm
  for term <- termsToCheck do
    val shadows = findInnerShadowingBindings(term, fieldBindingNames)
    for name <- shadows do
      report.error(
        s"Inner binding '$name' in the guard or body shadows pattern variable '$name'. " +
          s"The name-based substitution would incorrectly replace inner references. " +
          s"Use a different name for the inner binding."
      )
      hasErrors = true

  // 3. Detect duplicate variable names across constructors in composite patterns.
  //    Scala's own checker usually catches this, but this is defense-in-depth.
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

  if hasErrors then
    report.errorAndAbort("Join pattern has shadowed variable bindings (see errors above).")

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
    selfRefName: String
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = extractConstructorData(patterns)
  checkPatternSubtypesOfM[M](typesData)
  checkForShadowedBindings(typesData, selfRefName, guard, rhsTerm, patterns)
  val (predicate, filters) = generateGuard(guard, typesData)
  val extractors = buildExtractorTuples[M](typesData, filters)

  val fieldBindings = typesData.flatMap(_._2)
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
    generateRhs[M, T](rhsTerm, fieldBindings, selfRefName).asExprOf[(LookupEnv, ActorRef[M]) => T]

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
  * @return
  *   a join pattern expression with empty pattern bins and extractors.
  */
private[code_generation] def generateWildcardPattern[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(guard: Option[quotes.reflect.Term], rhsTerm: quotes.reflect.Term): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val (predicate, filter) = generateGuard(guard, List())
  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] = '{ (_: LookupEnv, _: ActorRef[M]) =>
    ${ rhsTerm.asExprOf[T] }
  }
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
  * @param selfRefName
  *   the name of the self ActorRef parameter.
  * @return
  *   an optional join pattern expression, or `None` if the pattern is unsupported.
  */
private[code_generation] def generateJoinPattern[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(
    joinPattern: quotes.reflect.CaseDef,
    selfRefName: String
): Option[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  joinPattern match
    case CaseDef(pattern, guard, rhsTerm) =>
      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, subPatterns), _) =>
          fun match
            case Select(_, "unapply") =>
              Some(generateJP[M, T](List(t), guard, rhsTerm, selfRefName))
            case TypeApply(Select(_, "unapply"), _) =>
              Some(generateJP[M, T](subPatterns, guard, rhsTerm, selfRefName))
        case andOperatorApplication @ Unapply(_, _, _) =>
          val patterns = getConstructorPatternsFromAndOps[M, T](andOperatorApplication)
          Some(generateJP[M, T](patterns, guard, rhsTerm, selfRefName))
        case w: Wildcard =>
          Some(generateWildcardPattern[M, T](guard, rhsTerm))
        case default =>
          errorTreeWithHint(
            "Unsupported case pattern",
            "Patterns must be case class constructors (e.g., `case Msg(x)`) or composite patterns (e.g., `case A(x) &:& B(y)`)",
            default
          )
          None
