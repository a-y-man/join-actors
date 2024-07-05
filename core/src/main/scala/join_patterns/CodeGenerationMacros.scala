package join_patterns

import actor.Actor
import actor.ActorRef
import actor.Result

import scala.collection.immutable.*
import scala.collection.immutable.TreeMap as MTree
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map as MutMap
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Varargs

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
  *   a list of tuples, containing pattern type representation, and a list of tuples, containing
  *   types' name and representation. substs: Map[String, Int]
  */
private def extractContructorData(using quotes: Quotes)(
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

/** Creates a guard function.
  *
  * @param guard
  *   the optional predicate.
  * @param inners
  *   the field types available in this pattern.
  * @return
  *   a `Block` that is the guard.
  */
private def generateGuard(using quotes: Quotes)(
    guard: Option[quotes.reflect.Term],
    inners: List[(String, quotes.reflect.TypeRepr)]
): quotes.reflect.Block =
  import quotes.reflect.*

  val _transform = new TreeMap:
    override def transformTerm(term: Term)(owner: Symbol): Term = super.transformTerm(term)(owner)

  var _rhsFn = (sym: Symbol, _: List[Tree]) =>
    _transform.transformTerm('{ true }.asExprOf[Boolean].asTerm.changeOwner(sym))(sym)

  guard match
    case Some(apply: Apply) =>
      if inners.isEmpty then
        _rhsFn = (sym: Symbol, _: List[Tree]) =>
          _transform.transformTerm(apply.changeOwner(sym))(sym)
      else
        _rhsFn = (sym: Symbol, params: List[Tree]) =>
          val p0 = params.head.asInstanceOf[Ident]

          val transform = new TreeMap:
            override def transformTerm(term: Term)(owner: Symbol): Term =
              term match
                case Ident(n) if inners.exists(_._1 == n) =>
                  val inner = '{ (${ p0.asExprOf[LookupEnv] })(${ Expr(n) }) }
                  inners.find(_._1 == n).get._2.asType match
                    case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
                case x =>
                  super.transformTerm(x)(owner)

          transform.transformTerm(apply.changeOwner(sym))(sym)

    case None          => ()
    case Some(default) => error("Unsupported guard", default, Some(default.pos))

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("_"))(_ => List(TypeRepr.of[LookupEnv]), _ => TypeRepr.of[Boolean]),
    rhsFn = _rhsFn
  )

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

  val typesData = extractContructorData(List(dataType))
  val extractors: List[(Expr[M => Boolean], Expr[M => LookupEnv])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          (
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => LookupEnv] }(m.asInstanceOf[ot])
            }
          )
    }.toList

  val (outer, inners) = typesData.head

  val patternInfo: Expr[PatternInfo[M]] = '{
    val _extractors  = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
    val checkMsgType = _extractors.head._1
    val extractField = _extractors.head._2

    PatternInfo(
      patternBins = MTree(List(0) -> MessageIdxs()),
      patternExtractors = PatternExtractors(0 -> (checkMsgType, extractField))
    )
  }

  outer.asType match
    case '[ot] =>
      val extract: Expr[
        List[M] => Option[PatternBins]
      ] =
        '{ (m: List[M]) =>
          val patInfo            = ${ patternInfo }
          val (_, patExtractors) = (patInfo.patternBins, patInfo.patternExtractors)
          val checkMsgType       = patExtractors(0)._1
          val extractField       = patExtractors(0)._2
          val messages           = ArrayBuffer.from(m.zipWithIndex)
          val (mQ, mQidx)        = messages.last // Take the newest msg from the queue

          if checkMsgType(mQ) then Some(PatternBins(PatternIdxs(0) -> MessageIdxs(mQidx)))
          else None
        }
      val predicate: Expr[LookupEnv => Boolean] =
        generateGuard(guard, inners).asExprOf[LookupEnv => Boolean]
      val rhs: Expr[(LookupEnv, ActorRef[M]) => T] =
        generateRhs[M, T](_rhs, inners, selfRef).asExprOf[(LookupEnv, ActorRef[M]) => T]
      val size = 1

      val updatedMTree = '{ (m: Tuple2[M, Int], pState: MatchingTree) =>
        val patInfo            = ${ patternInfo }
        val (_, patExtractors) = (patInfo.patternBins, patInfo.patternExtractors)
        val checkMsgType       = patExtractors(0)._1
        val (mQ, mQidx)        = m // Take the newest msg from the queue
        val mTree              = pState
        val mIdxs              = MessageIdxs(mQidx)
        if checkMsgType(mQ) then Some(mTree.updated(mIdxs, MTree(PatternIdxs(0) -> mIdxs)))
        else Some(mTree)
      }

      '{
        JoinPattern(
          $extract,
          $predicate,
          $rhs,
          ${ Expr(size) },
          $updatedMTree,
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

  val typesData = extractContructorData(dataType)
  val extractors: List[(Expr[String], Expr[M => Boolean], Expr[M => LookupEnv])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          (
            Expr(TypeTree.of[ot].symbol.name),
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => LookupEnv] }(m.asInstanceOf[ot])
            }
          )
    }.toList

  val (outers, inners) = (typesData.map(_._1), typesData.map(_._2).flatten)

  val patExtractors: Expr[PatternExtractors[M]] = '{
    val _extractors = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
    _extractors.zipWithIndex.map { case ((_, checkMsgType, extractField), idx) =>
      idx -> (checkMsgType, extractField)
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
          indices -> MessageIdxs()
        }

    PatternInfo(patternBins = patBins.to(MTree), patternExtractors = $patExtractors)
  }

  val extract: Expr[
    List[M] => Option[PatternBins]
  ] =
    '{ (m: List[M]) =>
      val messages    = m.zipWithIndex
      val _extractors = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
      val msgPatterns = _extractors.zipWithIndex

      def getMsgIdxsWithFits(
          messages: List[(M, Int)],
          msgPatterns: List[((String, M => Boolean, M => LookupEnv), Int)]
      ): List[(MessageIdx, PatternIdxs)] =
        messages
          .flatMap { case (msg, idx) =>
            val matches =
              msgPatterns.filter { case ((_, checkMsgType, _), _) => checkMsgType(msg) }.map(_._2)
            List((idx, matches))
          }
          .filter(_._2.nonEmpty)

      def buildPatternBins(
          messageIdxWithFits: List[(MessageIdx, PatternIdxs)],
          initialPatternBins: PatternBins
      ): PatternBins =
        messageIdxWithFits.foldLeft(initialPatternBins) { case (acc, (messageIdx, patternShape)) =>
          acc.updatedWith(patternShape) {
            case Some(messageIdxs) =>
              if messageIdxs.contains(messageIdx) then Some(messageIdxs)
              else Some(messageIdxs enqueue messageIdx)
            case None => Some(MessageIdxs())
          }
        }

      def isPatternBinComplete(
          patternBins: PatternBins
      ): Boolean =
        patternBins.forall((patShape, msgIdxs) => msgIdxs.size >= patShape.size)

      val messageIdxWithFits = getMsgIdxsWithFits(messages, msgPatterns)
      val initPatternBins    = ${ patternInfo }.patternBins
      val patternBins        = buildPatternBins(messageIdxWithFits, initPatternBins)

      if !isPatternBinComplete(patternBins) then None
      else Some(patternBins)
    }

  val predicate: Expr[LookupEnv => Boolean] =
    generateGuard(guard, inners).asExprOf[LookupEnv => Boolean]
  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] =
    generateRhs[M, T](_rhs, inners, self).asExprOf[(LookupEnv, ActorRef[M]) => T]
  val size = outers.size

  val updatedMTree: Expr[
    (
        Tuple2[M, Int],
        MatchingTree
    ) => Option[MatchingTree]
  ] =
    '{ (m: Tuple2[M, Int], mTree: MatchingTree) =>
      val (mQ, mQidx) = m // Take the newest msg from the queue
      val patInfo     = ${ patternInfo }

      val _extractors       = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
      val msgTypesInPattern = _extractors.map(pat => (pat._2, pat._3)).zipWithIndex

      val isMsgInPat = msgTypesInPattern.exists { case ((checkMsgType, _), _) => checkMsgType(mQ) }

      if isMsgInPat then
        val matches = msgTypesInPattern
          .filter { case ((checkMsgType, _), _) =>
            checkMsgType(mQ)
          }
          .map(_._2)

        val updatedMTree = updateMTree(mTree, mQidx, matches)
        Some(updatedMTree)
      else Some(mTree)
    }

  '{
    JoinPattern(
      $extract,
      $predicate,
      $rhs,
      ${ Expr(size) },
      $updatedMTree,
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

  val extract: Expr[
    List[M] => Option[PatternBins]
  ] = '{ (m: List[M]) =>
    None
  }
  val predicate: Expr[LookupEnv => Boolean] =
    generateGuard(guard, List()).asExprOf[LookupEnv => Boolean]
  val rhs: Expr[(LookupEnv, ActorRef[M]) => T] = '{ (_: LookupEnv, _: ActorRef[M]) =>
    ${ _rhs.asExprOf[T] }
  }
  val size = 1

  val updatedMTree = '{ (m: Tuple2[M, Int], pState: MatchingTree) =>
    None
  }

  val patternInfo: Expr[PatternInfo[M]] = '{
    PatternInfo(
      patternBins = MTree(),
      patternExtractors = Map()
    )
  }

  '{
    JoinPattern(
      $extract,
      $predicate,
      $rhs,
      ${ Expr(size) },
      $updatedMTree,
      ${ patternInfo }
    )
  }

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
        case w: Wildcard =>
          // report.info("Wildcards should be defined last", w.asExpr)
          Some(generateWildcardPattern[M, T](guard, _rhs))
        case default =>
          errorTree("Unsupported case pattern", default)
          None

/** Translates a series of match clauses into a list of join pattern.
  *
  * @param expr
  *   the match expression.
  * @return
  *   a list of join pattern expressions.
  */
private def getJoinDefinition[M, T](
    expr: Expr[(M, ActorRef[M]) => T]
)(using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  expr.asTerm match
    case Inlined(_, _, Block(_, Block(stmts, _))) =>
      stmts.head match
        case DefDef(_, List(TermParamClause(params)), _, Some(Block(_, Match(_, cases)))) =>
          val selfRef = params(1).name
          cases.flatMap(`case` => generateJoinPattern[M, T](`case`, selfRef))
        case default =>
          errorTree("Unsupported code", default)
          List()
    case default =>
      errorTree("Unsupported expression", default)
      List()

private def getJoinDefinition_[M, T](
    expr: Expr[ActorRef[M] => PartialFunction[Any, T]]
)(using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  expr.asTerm match
    case Inlined(_, _, Block(_, Block(stmts, _))) =>
      stmts.head match
        case DefDef(_, List(TermParamClause(params)), _, Some(Block(_, Block(body, _)))) =>
          body.head match
            case DefDef(_, _, _, Some(Match(_, cases))) =>
              val selfRef = params.head.name
              val jps     = cases.flatMap(`case` => generateJoinPattern[M, T](`case`, selfRef))
              // jps foreach { jp =>
              //   report.info(s"JP: ${jp.asTerm.show(using Printer.TreeShortCode)}")
              // }
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
    expr: Expr[(M, ActorRef[M]) => T]
)(using tm: Type[M], tt: Type[T], quotes: Quotes) = '{ (algorithm: MatchingAlgorithm) =>
  SelectMatcher[M, T](algorithm, ${ Expr.ofList(getJoinDefinition(expr)) })
}

private def receiveCodegen_[M, T](
    expr: Expr[ActorRef[M] => PartialFunction[Any, Result[T]]]
)(using
    tm: Type[M],
    tt: Type[T],
    quotes: Quotes
): Expr[MatchingAlgorithm => Matcher[M, Result[T]]] =
  import quotes.reflect.*

  val ret = '{ (algorithm: MatchingAlgorithm) =>
    SelectMatcher[M, Result[T]](
      algorithm,
      ${
        Expr.ofList(
          getJoinDefinition_(
            expr.asInstanceOf[Expr[ActorRef[M] => PartialFunction[Any, Result[T]]]]
          )
        )
      }
    )
  }

  // report.info(s"Generated code: ${ret.asTerm.show(using Printer.TreeAnsiCode)}", expr.asTerm.pos)
  ret

@deprecated("Use `receive` instead")
inline def receiveOld[M, T](inline f: (M, ActorRef[M]) => T): MatchingAlgorithm => Matcher[M, T] =
  ${ receiveCodegen('f) }

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
  ${ receiveCodegen_('f) }
