package join_patterns

import java.util.concurrent.LinkedTransferQueue as Queue
import scala.quoted.{Expr, Quotes, Type, Varargs}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

/** Extracts a type's name and representation from a `Tree`.
  *
  * @param t
  *   the tree, either a `Bind` or a `Typed`.
  * @return
  *   a tuple containing the type's name and representation.
  */
private def extractInner(using quotes: Quotes)(
    t: quotes.reflect.Tree
): (String, quotes.reflect.TypeRepr) =
  import quotes.reflect.*

  t match
    case Bind(n, typed @ Typed(_, TypeIdent(_))) => (n, typed.tpt.tpe.dealias.simplified)
    case typed @ Typed(Wildcard(), TypeIdent(_)) => ("_", typed.tpt.tpe.dealias.simplified)
    case _ =>
      errorTree("Unsupported bottom-level pattern", t)
      ("", TypeRepr.of[Nothing])

// add support for wilcard types (_, A, B, C), they must be checked last !!!!!
/** Extracts types' name and representation from patterns.
  *
  * @param patterns
  *   the patterns, as a `List[Tree]`.
  * @return
  *   a list of tuples, containing pattern type representation, and a list of tuples, containing
  *   types' name and representation.
  */
private def getTypesData(using quotes: Quotes)(
    patterns: List[quotes.reflect.Tree]
): List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])] =
  import quotes.reflect.*

  patterns.map {
    case TypedOrTest(Unapply(Select(_, "unapply"), _, binds), tt: TypeTree) =>
      tt.tpe.dealias.simplified match
        case tp: TypeRef => tp -> binds.map(extractInner(_))
    case default =>
      errorTree("Unsupported top-level pattern", default)
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
    tpe = MethodType(List(""))(_ => List(outerType), _ => TypeRepr.of[Map[String, Any]]),
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
      ('{ Map[String, Any](${ Varargs[(String, Any)](args) }: _*) }).asTerm
  )

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

  val _transform = new TreeMap {
    override def transformTerm(term: Term)(owner: Symbol): Term = super.transformTerm(term)(owner)
  }

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
          // report.info(s"generateGuard:transformTerm ---> ${p0.asExpr.show}")

          val transform = new TreeMap {
            override def transformTerm(term: Term)(owner: Symbol): Term =
              term match
                case Ident(n) if inners.exists(_._1 == n) =>
                  val inner = '{ (${ p0.asExprOf[Map[String, Any]] })(${ Expr(n) }) }
                  inners.find(_._1 == n).get._2.asType match
                    case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
                case x =>
                  super.transformTerm(x)(owner)
          }

          transform.transformTerm(apply.changeOwner(sym))(sym)

    case None          => ()
    case Some(default) => error("Unsupported guard", default, Some(default.pos))

  Lambda(
    owner = Symbol.spliceOwner,
    tpe =
      MethodType(List("_"))(_ => List(TypeRepr.of[Map[String, Any]]), _ => TypeRepr.of[Boolean]),
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
private def generateRhs[T](using
    quotes: Quotes,
    tt: Type[T]
)(rhs: quotes.reflect.Term, inners: List[(String, quotes.reflect.TypeRepr)]): quotes.reflect.Block =
  import quotes.reflect.*

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("_"))(_ => List(TypeRepr.of[Map[String, Any]]), _ => TypeRepr.of[T]),
    rhsFn = (sym: Symbol, params: List[Tree]) =>
      val p0 = params.head.asInstanceOf[Ident]
      val transform = new TreeMap {
        override def transformTerm(term: Term)(owner: Symbol): Term = term match
          case Ident(n) if inners.exists(_._1 == n) =>
            val inner = '{ (${ p0.asExprOf[Map[String, Any]] })(${ Expr(n) }) }
            inners.find(_._1 == n).get._2.asType match
              case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
          case x => super.transformTerm(x)(owner)
      }

      transform.transformTerm(rhs.changeOwner(sym))(sym)
  )

/** Generates a join-pattern for singleton patterns e.g. A(*) the asterix represents a potential
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
  *   A join-pattern for a singleton pattern
  */
private def generateSingletonPattern[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    dataType: quotes.reflect.Tree,
    guard: Option[quotes.reflect.Term],
    _rhs: quotes.reflect.Term
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = getTypesData(List(dataType))
  val extractors: List[(Expr[M => Boolean], Expr[M => Map[String, Any]])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          (
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => Map[String, Any]] }(m.asInstanceOf[ot])
            }
          )
    }.toList

  val (outer, inners) = typesData.head

  outer.asType match
    case '[ot] =>
      val extractor = generateExtractor(outer, inners.map(_._1))
        .asExprOf[ot => Map[String, Any]]
      val extract: Expr[List[M] => (Option[List[M]], List[(Int, M)], Map[String, Any])] =
        '{ (m: List[M]) =>
          m.find(_.isInstanceOf[ot]) match
            case None => (None, List(), Map())
            case Some(message) =>
              (
                Some(List(message)),
                List((m.indexOf(message), message)),
                $extractor(message.asInstanceOf[ot])
              )
        }
      val predicate: Expr[Map[String, Any] => Boolean] =
        generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
      val rhs: Expr[Map[String, Any] => T] =
        generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
      val size = 1

      // val fieldExtractor = extractor.asExprOf[M => Map[String, Any]]
      val partialExtract = '{ (m: List[M], mTree: MatchingTree[M]) =>
        val _extractors    = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
        val fieldExtractor = _extractors.head._2
        val checkMsgType   = _extractors.head._1
        val messages       = ListBuffer.from(m.zipWithIndex)
        val (mQ, mQidx)    = messages.last // Take the newest msg from the queue
        if checkMsgType(mQ) then
          Some(
            MatchingTree(
              mTree.nodeMapping + (List(mQidx) -> Set((0, checkMsgType, fieldExtractor)))
            )
          )
        else Some(mTree)
      }

      '{ JoinPattern($extract, $predicate, $rhs, ${ Expr(size) }, $partialExtract) }

/** Generates a join-pattern for composite patterns e.g. (A(*), B(*), C(*), ...) the asterix
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
  *   A join-pattern for a composite pattern
  */
private def generateCompositePattern[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    dataType: List[quotes.reflect.Tree],
    guard: Option[quotes.reflect.Term],
    _rhs: quotes.reflect.Term
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = getTypesData(dataType)
  val extractors: List[(Expr[M => Boolean], Expr[M => Map[String, Any]])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          (
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => Map[String, Any]] }(m.asInstanceOf[ot])
            }
          )
    }.toList

  val extract: Expr[List[M] => (Option[List[M]], List[(Int, M)], Map[String, Any])] =
    '{ (m: List[M]) =>
      val messages                      = ListBuffer.from(m.zipWithIndex)
      val matched: ListBuffer[(Int, M)] = ListBuffer()
      val fields: MutMap[String, Any]   = MutMap()
      val _extractors                   = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
      val msgPattern: ListBuffer[M]     = ListBuffer()

      val msgPatterns = _extractors
      for
        (msg, field) <- msgPatterns
        if matched.size < _extractors.size
      do
        messages.find((_m, _) => msg(_m)) match
          case Some((matchedMessage, idx)) =>
            matched.addOne((idx, matchedMessage))
            msgPattern.addOne(matchedMessage)
            messages.subtractOne((matchedMessage, idx))
            fields.addAll(field(matchedMessage))
          case None => ()

      if matched.size == _extractors.size then
        val patternVectorEntry = Some(msgPattern.toList)
        (patternVectorEntry, matched.toList, fields.toMap)
      else (None, List(), Map())
    }
  val (outers, inners) = (typesData.map(_._1), typesData.map(_._2).flatten)
  val predicate: Expr[Map[String, Any] => Boolean] =
    generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
  val rhs: Expr[Map[String, Any] => T] =
    generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
  val size = outers.size

  val partialExtract: Expr[
    (List[M], MatchingTree[M]) => Option[MatchingTree[M]]
  ] =
    '{ (m: List[M], mTree: MatchingTree[M]) =>
      val messages          = ListBuffer.from(m.zipWithIndex)
      val _extractors       = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
      val msgTypesInPattern = _extractors.zipWithIndex

      val (mQ, mQidx) = messages.last // Take the newest msg from the queue

      val isMsgInPat = msgTypesInPattern.exists { msgPat =>
        val ((isMsgMatch, _), _) = msgPat
        isMsgMatch(mQ)
      }

      if isMsgInPat then
        val matches = msgTypesInPattern
          .filter { msgPat =>
            val ((msg, fieldExtractor), _) = msgPat
            msg(mQ)
          }

        val newNodeMapping = mTree.nodeMapping.foldLeft(NodeMapping[M]()) { (acc, mapping) =>
          val (node, currentFits) = mapping

          val _matches = matches.map { msgPat =>
            val ((msgTypeChecker, fieldExtractor), msgPosInPat) = msgPat
            (msgPosInPat, msgTypeChecker, fieldExtractor)
          }.toSet

          val newFitsIdxs = _matches.map(_._1).diff(currentFits.map(_._1))
          if newFitsIdxs.isEmpty then acc + mapping
          else
            val currentFitsIdxs = currentFits.map(_._1)
            val newMappingIdxs  = currentFitsIdxs.`+`(newFitsIdxs.head)
            val newMapping = newMappingIdxs.map { idx =>
              val ((msgTypeChecker, fieldExtractor), _) = msgTypesInPattern(idx)
              (idx, msgTypeChecker, fieldExtractor)
            }
            if node.nonEmpty && currentFits.isEmpty then
              acc + ((node.appended(mQidx))    -> Set.empty)
            else acc + ((node.appended(mQidx)) -> newMapping) + mapping
        }
        Some(MatchingTree(newNodeMapping))
      else
        Some(
          MatchingTree(
            mTree.nodeMapping + (List(mQidx) -> Set.empty)
          )
        )
    }

  '{ JoinPattern($extract, $predicate, $rhs, ${ Expr(size) }, $partialExtract) }

/** Generates a join-pattern for a pattern written as a wildcard e.g. (_)
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
  *   A join-pattern for a wildcard pattern
  */
private def generateWildcardPattern[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(guard: Option[quotes.reflect.Term], _rhs: quotes.reflect.Term): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val extract: Expr[List[M] => (Option[List[M]], List[(Int, M)], Map[String, Any])] = '{
    (m: List[M]) => (None, List(), Map())
  }
  val predicate: Expr[Map[String, Any] => Boolean] =
    generateGuard(guard, List()).asExprOf[Map[String, Any] => Boolean]
  val rhs: Expr[Map[String, Any] => T] = '{ (_: Map[String, Any]) => ${ _rhs.asExprOf[T] } }
  val size                             = 1

  val partialExtract = '{ (m: List[M], mTree: MatchingTree[M]) => None }

  '{ JoinPattern($extract, $predicate, $rhs, ${ Expr(size) }, $partialExtract) }

/** Creates a join-pattern from a `CaseDef`.
  *
  * @param case
  *   the source `CaseDef`.
  * @return
  *   a join-pattern expression.
  */
private def generateJoinPattern[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    `case`: quotes.reflect.CaseDef
): Option[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  `case` match
    case CaseDef(pattern, guard, _rhs) =>
      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, patterns), _) =>
          fun match
            case Select(_, "unapply") =>
              Some(generateSingletonPattern[M, T](t, guard, _rhs))
            case TypeApply(Select(_, "unapply"), _) =>
              Some(generateCompositePattern[M, T](patterns, guard, _rhs))
        case w: Wildcard =>
          // report.info("Wildcards should be defined last", w.asExpr)
          Some(generateWildcardPattern[M, T](guard, _rhs))
        case default =>
          errorTree("Unsupported case pattern", default)
          None

/** Translates a series of match clauses into a list of join-pattern.
  *
  * @param expr
  *   the match expression.
  * @return
  *   a list of join-pattern expressions.
  */
private def getCases[M, T](
    expr: Expr[M => T]
)(using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*

  expr.asTerm match
    case Inlined(_, _, Block(_, Block(stmts, _))) =>
      stmts.head match
        case DefDef(_, _, _, Some(Block(_, Match(_, cases)))) =>
          cases.flatMap { generateJoinPattern[M, T](_) }

        // report.info(
        //   f"Generated code: ${Expr.ofList(code).asTerm.show(using Printer.TreeAnsiCode)}"
        // )
        // code
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
    expr: Expr[M => T]
)(using tm: Type[M], tt: Type[T], quotes: Quotes) = '{ (algorithm: MatchingAlgorithm) =>
  SelectMatcher[M, T](algorithm, ${ Expr.ofList(getCases(expr)) })
}

/** Entry point of the `receive` macro.
  *
  * @param f
  *   the block to use as source of the pattern-matching code.
  * @return
  *   a compile-time closure that takes a MatchingAlgorithm type
  *   and returns a Matcher-object that performs pattern-matching
  *   on a message queue at runtime.
  */
inline def receive[M, T](inline f: M => T): MatchingAlgorithm => Matcher[M, T] =
  ${ receiveCodegen('f) }
