package join_patterns.code_generation

import com.google.common.collect.LinkedHashMultiset
import join_actors.actor.{ActorRef, Result}
import join_patterns.matching.*
import join_patterns.types.{*, given}
import join_patterns.util.*

import java.util.stream.Collectors
import scala.collection
import scala.collection.immutable.{TreeMap as MTree, *}
import scala.quoted.{Expr, Quotes, Type, Varargs}
import scala.jdk.StreamConverters.*

object `&&&`:
  infix def unapply(arg: Any): Option[(Unit, Unit)] = Some((), ())

/** Extracts a type's name and representation from a `Tree`.
  *
  * @param t
  *   the tree, either a `Bind` or a `Typed`.
  * @return
  *   a tuple containing the type's name and representation.
  */
private def extractPayloads(using quotes: Quotes)(
    t: quotes.reflect.Tree
): (String, quotes.reflect.TypeRepr) =
  import quotes.reflect.*

  t match
    case Bind(n, typed @ Typed(_, TypeIdent(_))) => (n, typed.tpt.tpe.dealias.simplified)
    case typed @ Typed(Wildcard(), TypeIdent(_)) => ("_", typed.tpt.tpe.dealias.simplified)
    case b @ Bind(n, typed @ Typed(Wildcard(), Applied(_, _))) =>
      (n, typed.tpt.tpe.dealias.simplified)
    case Bind(n, w @ Wildcard()) => (n, w.tpe.dealias.simplified)
    case w @ Wildcard()          => ("_", w.tpe.dealias.simplified)
    case e =>
      errorTree(s"Unsupported payload type", t)
      ("", TypeRepr.of[Nothing])

/** Extracts types' name and representation from patterns.
  *
  * @param patterns
  *   the patterns, as a `List[Tree]`.
  * @return
  *   a list of tuples where the first element is a constructor type representation, and the second
  *   element is a list of tuples containing the names and type representations of bound variables
  */
private def extractConstructorData(using quotes: Quotes)(
    patterns: List[quotes.reflect.Tree]
): List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])] =
  import quotes.reflect.*

  patterns.map {
    case TypedOrTest(Unapply(Select(s, "unapply"), _, binds), tt: TypeTree) =>
      tt.tpe.dealias.simplified match
        case tp: TypeRef =>
          tp -> binds.map(extractPayloads(_))
    case default =>
      errorTree("Unsupported message constructor type", default)
      TypeRepr.of[Nothing] -> List()
  }

/** Creates an extractor function.
  *
  * @param outerType
  *   the message type.
  * @param varNames
  *   the field names.
  * @return
  *   a `Block` that is the extractor.
  */
private def generateExtractor(using
    quotes: Quotes
)(outerType: quotes.reflect.TypeRepr, varNames: List[String]): quotes.reflect.Block =
  import quotes.reflect.*

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List(""))(_ => List(outerType), _ => TypeRepr.of[LookupEnv]),
    rhsFn = (_: Symbol, params: List[Tree]) =>
      val p0 = params.head.asInstanceOf[Ident]
      val isMemberName: Symbol => Boolean =
        (p: Symbol) => p.name.head == '_' && p.name.tail.toIntOption.isDefined
      val memberSymbols: List[Symbol] = outerType.typeSymbol.methodMembers
        .filter(isMemberName(_))
        .sortBy(_.name)
      val args = varNames.zipWithIndex.map { (name, i) =>
        Expr.ofTuple(Expr(name), Select(p0, memberSymbols(i)).asExprOf[Any])
      }
      ('{ LookupEnv(${ Varargs[(String, Any)](args) }*) }).asTerm
  )

private def substitute(using quotes: Quotes)(
    rhs: quotes.reflect.Term,
    identToBeReplaced: String,
    replacementExpr: quotes.reflect.Term
)(sym: quotes.reflect.Symbol): quotes.reflect.Term =
  import quotes.reflect.*

  val transform = new TreeMap:
    override def transformTerm(term: Term)(owner: Symbol): Term =
      term match
        case t: Ident if identToBeReplaced == t.name =>
          replacementExpr.changeOwner(owner)
        case x =>
          super.transformTerm(x)(owner)

  transform.transformTerm(rhs.changeOwner(sym))(sym)

private def substInners[T](using quotes: Quotes, tt: Type[T])(
    inners: List[(String, quotes.reflect.TypeRepr)],
    rhs: quotes.reflect.Term,
    substs: quotes.reflect.Term
)(owner: quotes.reflect.Symbol): quotes.reflect.Term =
  import quotes.reflect.*

  val result = inners.foldLeft(rhs) { case (acc, (name, tpe)) =>
    val replacementExpr = '{ (${ substs.asExprOf[LookupEnv] })(${ Expr(name) }) }
    tpe.asType match
      case '[innerType] =>
        val x = ('{ ${ replacementExpr }.asInstanceOf[innerType] }).asTerm
        substitute(acc, name, x)(owner)
  }
  result

/**
 * Given a Boolean expression that is a conjunction of clauses a && b && c &&..., returns a list of the clauses
 * List(a, b, c...)
 */
private def extractClauses(using quotes: Quotes)(
  expr: Expr[Boolean]
): List[Expr[Boolean]] =
  import quotes.reflect.*

  expr match
    case '{ ($a: Boolean) && ($b: Boolean) } =>
      extractClauses(a) ::: extractClauses(b)
    case b =>
      List(b)

/**
 * Given a list of Boolean clauses, constructs a Boolean expression that is a conjunction of those clauses. The reverse
 * of extractClauses
 */
private def reconstructConjunctionTree(using quotes: Quotes)(
  clauses: List[Expr[Boolean]]
): Expr[Boolean] =
  clauses match
    case Nil => '{ true }
    case lst => lst.reduceLeft { (acc, exp) => '{$acc && $exp} }

/**
 * Returns all variable names used inside of a Term
 */
private def getAllVariableNames(using quotes: Quotes)(term: quotes.reflect.Term): List[String] =
  import quotes.reflect.*

  val folder = new TreeAccumulator[List[String]]:
    override def foldTree(acc: List[String], tree: Tree)(owner: Symbol): List[String] =
      tree match
        case Ident(name) => name :: acc
        case e => foldOverTree(acc, e)(owner)

  folder.foldTree(List(), term)(Symbol.spliceOwner)

type GuardFilter = LookupEnv => Boolean

private def replaceInnersWithLookupEnv[T](using quotes: Quotes, tt: Type[T])(
  exp: Expr[T],
  inners: List[(String, Type[?])],
  lookupEnvExpr: Expr[LookupEnv]
): Expr[T] =
  import quotes.reflect.*

  val transform = new TreeMap:
    override def transformTerm(term: Term)(owner: Symbol): Term =
      term match
        case Ident(n) if inners.exists(_._1 == n) =>
          val inner = '{ (${ lookupEnvExpr })(${ Expr(n) }) }
          inners.find(_._1 == n).get._2 match
            case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
        case x =>
          super.transformTerm(x)(owner)

  transform.transformTerm(exp.asTerm)(Symbol.spliceOwner).asExprOf[T]

/** Creates a guard function.
  *
  * @param guard
  *   the optional predicate.
  * @param typesData
  *   The outer type representations of this pattern, together with a list of inner type names and type representations
  * @return
  *   a `Block` that is the guard.
  */
private def generateGuard(using quotes: Quotes)(
    guard: Option[quotes.reflect.Term],
    typesData: List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])]
): (Expr[GuardFilter], Map[String, Expr[GuardFilter]]) =
  import quotes.reflect.*

  val inners = typesData.flatMap(_._2)
  val innersForSplice = inners.map((n, t) => (n, t.asType))

  val emptyFilteringLambdas = Map[String, Expr[GuardFilter]]()

  guard match
    case None          => ('{ (_: LookupEnv) => true }, emptyFilteringLambdas)
    case Some(term: Term) =>
      if inners.isEmpty then
        ('{ (_: LookupEnv) => ${ term.asExprOf[Boolean] }}, emptyFilteringLambdas)
      else
        val clauses = extractClauses(term.asExprOf[Boolean])

//        for c <- clauses do
//          println(c.asTerm)
//          println(s"\t${getAllVariableNames(c.asTerm)}")

        val clausesAndVariableNames =
          for c <- clauses yield
            (c, getAllVariableNames(c.asTerm))

        val typeNamesAndVariables = typesData.map((repr, lst) => (repr.typeSymbol.name, lst.map(_._1)))

        val typesAppearingOnce = typeNamesAndVariables
          .asJavaSeqStream
          .map(_._1)
          .collect(Collectors.toCollection(() => LinkedHashMultiset.create[String]()))
          .entrySet()
          .stream()
          .filter(_.getCount == 1)
          .map(_.getElement)
          .toScala(Seq)

//        println(s"All types: ${typesData.map(_._1.typeSymbol.name)}")
//        println(s"Types appearing once: $typesAppearingOnce")

        val typeNamesAndFilteringClauses =
          for t <- typesAppearingOnce yield
            val clauses =
              // Find the names of bound variables in all other outers
              val variablesFromOthers = typeNamesAndVariables.iterator
                .filter(_._1 != t)
                .flatMap(_._2)
                .toList

              // Find clauses where no bound variables are taken from the other outers
              for
                (c, clauseVars) <- clausesAndVariableNames
                if clauseVars.forall(!variablesFromOthers.contains(_))
              yield c

            t -> clauses

//        println(s"Type names and filtering clauses: ${typeNamesAndFilteringClauses.map{ (t, c) => (t, c.map(_.show)) }}")

        val typeNamesAndFilterExpressions = typeNamesAndFilteringClauses.map: (t, cs) =>
          (t, reconstructConjunctionTree(cs))

//        println(s"Type names and filter expressions: ${typeNamesAndFilterExpressions.map((t, e) => (t, e.show))}")

        // Construct filtering lambdas
        val filteringLambdas = typeNamesAndFilterExpressions.iterator
          .map { (t, exp) =>
            val lambda = '{ (lookupEnv: LookupEnv) => ${ replaceInnersWithLookupEnv(exp, innersForSplice, 'lookupEnv) }}

            (t, lambda)
          }
          .toMap

//        println(s"Type names and filter lambdas: ${filteringLambdas.map((t, e) => (t, e.show))}")


//        val filteringClauses = typeNamesAndFilteringClauses.flatMap(_._2)
//        val finalGuardClauses = clauses.filterNot(filteringClauses.contains(_))
//        val finalGuardExpression = reconstructConjunctionTree(finalGuardClauses)

        val guardLambda = '{ (lookupEnv: LookupEnv) => ${ replaceInnersWithLookupEnv(term.asExprOf[Boolean], innersForSplice, 'lookupEnv) }}

        (guardLambda, filteringLambdas)


/** Creates the right-hand side function.
  *
  * @param rhs
  *   the right-hand side.
  * @param inners
  *   the field types available in this pattern.
  * @return
  *   a `Block` that is the rhs.
  */
private def generateRhs[M, T](using
    quotes: Quotes,
    tt: Type[T],
    tm: Type[M]
)(
    rhs: quotes.reflect.Term,
    inners: List[(String, quotes.reflect.TypeRepr)],
    selfRef: String
): quotes.reflect.Block =
  import quotes.reflect.*

  val transformed =
    Lambda(
      owner = Symbol.spliceOwner,
      tpe = MethodType(List("_", s"$selfRef"))(
        _ =>
          List(
            TypeRepr.of[LookupEnv],
            TypeRepr.of[ActorRef[M]]
          ), // rhsFn takes 2 params: LookupEnv and ActorRef[M]
        _ => TypeRepr.of[T]
      ),
      rhsFn = (sym: Symbol, params: List[Tree]) =>
        val lookupEnv   = params.head.asInstanceOf[Ident]
        val actorRefObj = params(1).asExprOf[ActorRef[M]].asTerm
        val rhsWithSelf = substitute(rhs, selfRef, actorRefObj)(sym)
        val transform = new TreeMap:
          override def transformTerm(term: Term)(owner: Symbol): Term = term match
            case Ident(n) if inners.exists(_._1 == n) =>
              val inner = '{ (${ lookupEnv.asExprOf[LookupEnv] })(${ Expr(n) }) }
              inners.find(_._1 == n).get._2.asType match
                case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
            case x => super.transformTerm(x)(owner)

        transform.transformTerm(rhsWithSelf.changeOwner(sym))(sym)
    )
  transformed

/** Generates a join pattern for unary patterns e.g. A(*) the asterix represents a potential
  * payload.
  * @param dataType
  *   The type of the case class representing the message.
  *
  * @param guard
  *   The guard of the pattern.
  *
  * @param _rhs
  *   The right-hand side of the pattern.
  *
  * @return
  *   A join pattern for a unary pattern
  */
private def generateUnaryJP[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    dataType: quotes.reflect.Tree,
    guard: Option[quotes.reflect.Term],
    _rhs: quotes.reflect.Term,
    selfRef: String
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = extractConstructorData(List(dataType))

  val (predicate, filters) = generateGuard(guard, typesData)

  val extractors: List[(Expr[M => Boolean], Expr[M => LookupEnv], Expr[GuardFilter])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          val filteringLambda = filters.getOrElse(TypeTree.of[ot].symbol.name, '{ (_: LookupEnv) => true })

          (
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => LookupEnv] }(m.asInstanceOf[ot])
            },
            filteringLambda
          )
    }.toList

  val (outer, inners) = typesData.head

  val patternInfo: Expr[PatternInfo[M]] = '{
    val _extractors  = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
    val checkMsgType = _extractors.head._1
    val extractField = _extractors.head._2
    val filterer = _extractors.head._3

    PatternInfo(
      patternBins = MTree(PatternIdxs(0) -> MessageIdxs()),
      patternExtractors = PatternExtractors(0 -> PatternIdxInfo(checkMsgType, extractField, filterer))
    )
  }

  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] =
    generateRhs[M, T](_rhs, inners, selfRef).asExprOf[(LookupEnv, ActorRef[M]) => T]
  val size = 1

  '{
    JoinPattern(
      $predicate,
      $rhs,
      ${ Expr(size) },
      ${ patternInfo }
    )
  }

/** Generates a join pattern for composite patterns e.g. (A(*), B(*), C(*), ...) the asterix
  * represents a potential payload.
  * @param dataType
  *   The type of the case class representing the message.
  *
  * @param guard
  *   The guard of the pattern.
  *
  * @param _rhs
  *   The right-hand side of the pattern.
  *
  * @return
  *   A join pattern for a composite pattern
  */
private def generateNaryJP[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    dataType: List[quotes.reflect.Tree],
    guard: Option[quotes.reflect.Term],
    _rhs: quotes.reflect.Term,
    self: String
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = extractConstructorData(dataType)

  val (predicate, filters) =
    generateGuard(guard, typesData)

  val extractors: List[(Expr[String], Expr[M => Boolean], Expr[M => LookupEnv], Expr[GuardFilter])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          val typeName = TypeTree.of[ot].symbol.name

          val filteringLambda = filters.getOrElse(TypeTree.of[ot].symbol.name, '{ (_: LookupEnv) => true })

          (
            Expr(typeName),
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => LookupEnv] }(m.asInstanceOf[ot])
            },
            filteringLambda
          )
    }.toList

  val (outers, inners) = (typesData.map(_._1), typesData.map(_._2).flatten)

  val patExtractors: Expr[PatternExtractors[M]] = '{
    val _extractors = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
    _extractors.zipWithIndex.map { case ((_, checkMsgType, extractField, filterer), idx) =>
      idx -> PatternIdxInfo(checkMsgType, extractField, filterer)
    }.toMap
  }

  val patternInfo: Expr[PatternInfo[M]] = '{
    val _extractors       = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
    val msgTypesInPattern = _extractors.map(pat => (pat._1, pat._2)).zipWithIndex
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
    generateRhs[M, T](_rhs, inners, self).asExprOf[(LookupEnv, ActorRef[M]) => T]
  val size = outers.size

  '{
    JoinPattern(
      $predicate,
      $rhs,
      ${ Expr(size) },
      ${ patternInfo }
    )
  }

/** Generates a join pattern for a pattern written as a wildcard e.g. (_)
  *
  * @param dataType
  *   The type of the case class representing the message.
  *
  * @param guard
  *   The guard of the pattern.
  *
  * @param _rhs
  *   The right-hand side of the pattern.
  *
  * @return
  *   A join pattern for a wildcard pattern
  */
private def generateWildcardPattern[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(guard: Option[quotes.reflect.Term], _rhs: quotes.reflect.Term): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val (predicate, filter) =
    generateGuard(guard, List())
  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] = '{ (_: LookupEnv, _: ActorRef[M]) =>
    ${ _rhs.asExprOf[T] }
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

private def getConstructorPatternsFromAndOps[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
  unapplyTree: quotes.reflect.Unapply
): List[quotes.reflect.TypedOrTest] =
  import quotes.reflect.*

  // Extract left and right children of the &&& unapply
  val (left, right) =
    unapplyTree match
      case Unapply(_fun, _implicits, left::right::List()) => (left, right)
      case err => throw MatchError(err)

  // Since &&& is left-associative, the right child will always be a TypedOrTest, so we can coerce it
  val rightTot = right.asInstanceOf[TypedOrTest]

  // Check case of left child
  left match
    // Base case, the left child is also a leaf
    case leftTot: TypedOrTest => List(leftTot, rightTot)

    // Recursive case, the left child is another unapply
    case leftUnapply: Unapply => getConstructorPatternsFromAndOps[M, T](leftUnapply) :+ rightTot

    case err => throw MatchError(err)

/** Creates a join pattern from a `CaseDef`.
  *
  * @param case
  *   the source `CaseDef`.
  * @return
  *   a join pattern expression.
  */
private def generateJoinPattern[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    joinPattern: quotes.reflect.CaseDef,
    selfRef: String
): Option[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  joinPattern match
    case CaseDef(pattern, guard, _rhs) =>
      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, patterns), _) =>
          fun match
            case Select(_, "unapply") =>
              Some(generateUnaryJP[M, T](t, guard, _rhs, selfRef))
            case TypeApply(Select(_, "unapply"), _) =>
              Some(generateNaryJP[M, T](patterns, guard, _rhs, selfRef))
        case andOperatorApplication @ Unapply(_, _, _) =>
          val patterns = getConstructorPatternsFromAndOps[M, T](andOperatorApplication)
          Some(generateNaryJP[M, T](patterns, guard, _rhs, selfRef))
        case w: Wildcard =>
          // report.info("Wildcards should be defined last", w.asExpr)
          Some(generateWildcardPattern[M, T](guard, _rhs))
        case default =>
          errorTree("Unsupported case pattern", default)
          None

/** Translates a series of match clauses into a list of join patterns.
  *
  * @param expr
  *   the match expression.
  * @return
  *   a list of join pattern expressions.
  */
private def getJoinDefinition[M, T](
    expr: Expr[ActorRef[M] => PartialFunction[Any, T]]
)(using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  expr.asTerm match
    case Inlined(_, _, Inlined(_, _, Block(_, Block(stmts, _)))) =>
      stmts.head match
        case DefDef(_, List(TermParamClause(params)), _, Some(Block(_, Block(body, _)))) =>
          body.head match
            case DefDef(_, _, _, Some(Match(_, cases))) =>
              val selfRef = params.head.name
              val jps     = cases.flatMap(`case` => generateJoinPattern[M, T](`case`, selfRef))
              jps
        case default =>
          errorTree("Unsupported code", default)
          List()
    case default =>
      errorTree("Unsupported expression", default)
      List()

/** Generate the code returned by the receive macro.
  *
  * @param expr
  *   the match expression.
  * @return
  *   a matcher instance.
  */
private def receiveCodegen[M, T](
    expr: Expr[ActorRef[M] => PartialFunction[Any, Result[T]]]
)(using
    tm: Type[M],
    tt: Type[T],
    quotes: Quotes
): Expr[MatchingAlgorithm => Matcher[M, Result[T]]] =
  import quotes.reflect.*

  '{ (algorithm: MatchingAlgorithm) =>
    SelectMatcher[M, Result[T]](
      algorithm,
      ${
        Expr.ofList(
          getJoinDefinition(
            expr.asInstanceOf[Expr[ActorRef[M] => PartialFunction[Any, Result[T]]]]
          )
        )
      }
    )
  }

/** Entry point of the `receive` macro.
  *
  * @param f
  *   the block to use as source of the pattern-matching code.
  * @return
  *   a compile-time closure that takes a MatchingAlgorithm type and returns a Matcher-object that
  *   performs pattern-matching on a message queue at runtime.
  */
inline def receive[M, T](
    inline f: (ActorRef[M] => PartialFunction[Any, Result[T]])
): MatchingAlgorithm => Matcher[M, Result[T]] =
  ${ receiveCodegen('f) }
