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
