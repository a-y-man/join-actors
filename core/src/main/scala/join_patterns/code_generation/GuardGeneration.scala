package join_patterns.code_generation

import join_patterns.types.*

import scala.quoted.{Expr, Quotes, Type}

/** Predicate type alias for guard filtering on extracted bindings. */
type GuardFilter = LookupEnv => Boolean

/** Decomposes a conjunction `a && b && c && ...` into a flat list of clauses `List(a, b, c, ...)`. */
private[code_generation] def extractClauses(using quotes: Quotes)(
    expr: Expr[Boolean]
): List[Expr[Boolean]] =
  import quotes.reflect.*

  expr match
    case '{ ($a: Boolean) && ($b: Boolean) } =>
      extractClauses(a) ::: extractClauses(b)
    case b =>
      List(b)

/** Reconstructs a conjunction expression from a list of Boolean clauses.
  * Inverse of `extractClauses`.
  */
private[code_generation] def reconstructConjunctionTree(using quotes: Quotes)(
    clauses: List[Expr[Boolean]]
): Expr[Boolean] =
  clauses match
    case Nil => '{ true }
    case lst => lst.reduceLeft { (acc, exp) => '{ $acc && $exp } }

/** Finds type names that appear exactly once in the pattern.
  * Types appearing once can have their guard clauses evaluated early for per-type rejection.
  */
private[code_generation] def findTypesAppearingOnce(
    typeNamesAndVariables: List[(String, List[String])]
): Seq[String] =
  typeNamesAndVariables
    .map(_._1)
    .groupBy(identity)
    .collect { case (name, occurrences) if occurrences.size == 1 => name }
    .toSeq

/** For each type appearing once, finds guard clauses that only reference variables
  * from that type. These clauses can be used for early rejection during matching,
  * without waiting for all message types to be available.
  */
private[code_generation] def buildFilteringClauses[Q <: Quotes](
    typesAppearingOnce: Seq[String],
    clausesAndVariableNames: List[(Expr[Boolean], List[String])],
    typeNamesAndVariables: List[(String, List[String])]
): Seq[(String, List[Expr[Boolean]])] =
  for t <- typesAppearingOnce yield
    val clauses =
      val variablesFromOthers = typeNamesAndVariables.iterator
        .filter(_._1 != t)
        .flatMap(_._2)
        .toList

      for
        (c, clauseVars) <- clausesAndVariableNames
        if clauseVars.forall(!variablesFromOthers.contains(_))
      yield c

    t -> clauses

/** Creates a guard function and per-type filtering lambdas from an optional guard predicate.
  *
  * The guard function evaluates the full guard predicate against a `LookupEnv`.
  * Per-type filtering lambdas allow early rejection of individual messages before
  * all messages in a composite pattern are available.
  *
  * @param guard
  *   the optional guard predicate term.
  * @param typesData
  *   constructor types and their field bindings.
  * @return
  *   a tuple of (full guard lambda, map of type name → per-type filtering lambda).
  */
private[code_generation] def generateGuard(using quotes: Quotes)(
    guard: Option[quotes.reflect.Term],
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])]
): (Expr[GuardFilter], Map[String, Expr[GuardFilter]]) =
  import quotes.reflect.*

  val fieldBindings = typesData.flatMap(_._2)
  val fieldBindingsWithTypes = fieldBindings.map((n, t) => (n, t.asType))

  val emptyFilteringLambdas = Map[String, Expr[GuardFilter]]()

  guard match
    case None => ('{ (_: LookupEnv) => true }, emptyFilteringLambdas)
    case Some(term: Term) =>
      if fieldBindings.isEmpty then
        ('{ (_: LookupEnv) => ${ term.asExprOf[Boolean] } }, emptyFilteringLambdas)
      else
        val clauses = extractClauses(term.asExprOf[Boolean])

        val clausesAndVariableNames =
          for c <- clauses yield (c, getAllVariableNames(c.asTerm))

        val typeNamesAndVariables =
          typesData.map((repr, lst) => (repr.typeSymbol.name, lst.map(_._1)))

        val typesAppearingOnce = findTypesAppearingOnce(typeNamesAndVariables)
        val typeNamesAndFilteringClauses =
          buildFilteringClauses(typesAppearingOnce, clausesAndVariableNames, typeNamesAndVariables)

        val typeNamesAndFilterExpressions = typeNamesAndFilteringClauses.map: (t, cs) =>
          (t, reconstructConjunctionTree(cs))

        val filteringLambdas = typeNamesAndFilterExpressions.iterator
          .map { (t, exp) =>
            val lambda = '{ (lookupEnv: LookupEnv) =>
              ${ replaceInnersWithLookupEnv(exp, fieldBindingsWithTypes, 'lookupEnv) }
            }
            (t, lambda)
          }
          .toMap

        val guardLambda = '{ (lookupEnv: LookupEnv) =>
          ${ replaceInnersWithLookupEnv(term.asExprOf[Boolean], fieldBindingsWithTypes, 'lookupEnv) }
        }

        (guardLambda, filteringLambdas)
