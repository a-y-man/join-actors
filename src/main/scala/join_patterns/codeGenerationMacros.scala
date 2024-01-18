package join_patterns

import actor.Actor
import actor.ActorRef

import java.util.concurrent.LinkedTransferQueue as Queue
import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.Varargs

// Bind("b",
//   TypedOrTest(Unapply(Select(Ident("Bid"), "unapply"), Nil,
//   List(Bind("bidName", Typed(Wildcard(), TypeIdent("String"))),
//        Bind("bidPrice", Typed(Wildcard(), TypeIdent("Int"))),
//        Bind("bidVal", Typed(Wildcard(), TypeIdent("Int"))),
//        Bind("bidder", Typed(Wildcard(), TypeIdent("String"))))), Inferred()))

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
    case b @ Bind(n, typed @ Typed(Wildcard(), Applied(_, _))) =>
      (n, typed.tpt.tpe.dealias.simplified)
    case Bind(n, w @ Wildcard()) => (n, w.tpe.dealias.simplified)
    // add support for binding patterns clauses using @
    // case Bind(binderName, Bind(boundName, typed @ Typed(_, TypeIdent(_)))) =>
    //   (binderName, typed.tpt.tpe.dealias.simplified)
    // case Bind(binderName, Bind(boundName, w @ Wildcard())) => (binderName, w.tpe.dealias.simplified)
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
    case TypedOrTest(Unapply(Select(s, "unapply"), _, binds), tt: TypeTree) =>
      // println(s"IDENT: ${classOf[s.companionClass]}") // '{ classOf[$s] }
      tt.tpe.dealias.simplified match
        case tp: TypeRef =>
          // println(s.symbol.companionClass.typeRef.dealias.simplified)
          tp -> binds.map(extractInner(_))
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
      ('{ Map[String, Any](${ Varargs[(String, Any)](args) }*) }).asTerm
  )

private def collectBindings(using quotes: Quotes)(
    tree: quotes.reflect.Tree
): List[quotes.reflect.Symbol] =
  import quotes.reflect.*

  tree match
    case Block(stmts, exprs) =>
      val collectVals = stmts.foldLeft(List.empty[Symbol]) { case (acc, stmt) =>
        stmt match
          case ValDef(_, _, _) =>
            stmt.symbol :: acc
          case _ => acc
      }
      collectVals
    case _ => Nil

private def substitute(using quotes: Quotes)(
    rhs: quotes.reflect.Term,
    identToBeReplaced: String,
    replacementExpr: quotes.reflect.Term
): quotes.reflect.Term =
  import quotes.reflect.*
  val boundSymbols = collectBindings(rhs)
  // report.info(
  //   s"${boundSymbols.map(_.name).mkString(", ")}"
  // )
  val sym = Symbol.spliceOwner
  val transform = new TreeMap:
    override def transformTerm(rhsBlock: Term)(owner: Symbol): Term =
      rhsBlock match
        case Ident(name)
            if !boundSymbols
              .exists(_.name equals name) && identToBeReplaced == name =>
          // report.info(
          //   s"Replacing $name with $replacementExpr in $identToBeReplaced ----- ${boundSymbols.map(_.name).mkString(", ")}"
          // )

          replacementExpr.changeOwner(sym)
        case x =>
          super.transformTerm(x)(owner)
  transform.transformTerm(rhs.changeOwner(sym))(sym)

@tailrec
private def substInners[T](using quotes: Quotes, tt: Type[T])(
    inners: List[(String, quotes.reflect.TypeRepr)],
    rhs: quotes.reflect.Term,
    substs: quotes.reflect.Term
): quotes.reflect.Term =
  import quotes.reflect.*

  if inners.isEmpty then rhs
  else
    val (name, tpe)     = inners.head
    val replacementExpr = '{ (${ substs.asExprOf[Map[String, Any]] })(${ Expr(name) }) }
    tpe.asType match
      case '[innerType] =>
        val x = ('{ ${ replacementExpr }.asInstanceOf[innerType] }).asTerm
        substInners(inners.tail, substitute(rhs, name, x), substs)

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
                  val inner = '{ (${ p0.asExprOf[Map[String, Any]] })(${ Expr(n) }) }
                  inners.find(_._1 == n).get._2.asType match
                    case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
                case x =>
                  super.transformTerm(x)(owner)

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
      tpe = MethodType(List("_", "_"))(
        _ =>
          List(
            TypeRepr.of[Map[String, Any]],
            TypeRepr.of[ActorRef[M]]
          ), // rhsFn takes 2 params: Map[String, Any] and ActorRef[M]
        _ => TypeRepr.of[T]
      ),
      rhsFn = (sym: Symbol, params: List[Tree]) =>
        val p0                 = params.head.asInstanceOf[Ident]
        val replaceSelf        = substitute(rhs, selfRef, (params(1).asExprOf[ActorRef[M]]).asTerm)
        val substInnersInBlock = substInners[T](inners, replaceSelf, p0)
        // report.info(s"generateRhs:transformTerm ---> ${Printer.TreeStructure.show(replaceSelf)})}")
        val transform = new TreeMap:
          override def transformTerm(term: Term)(owner: Symbol): Term =
            // report.info(Printer.TreeStructure.show(term))
            term match
              case x => super.transformTerm(x)(owner)

        transform.transformTerm(substInnersInBlock.changeOwner(sym))(sym)
    )
  transformed

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
    _rhs: quotes.reflect.Term,
    selfRef: String
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
      val extract: Expr[
        List[M] => Option[(Iterator[List[Int]], Set[((M => Boolean, M => Map[String, Any]), Int)])]
      ] =
        '{ (m: List[M]) =>
          val _extractors  = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
          val extractField = _extractors.head._2
          val checkMsgType = _extractors.head._1
          val messages     = ListBuffer.from(m.zipWithIndex)
          val (mQ, mQidx)  = messages.last // Take the newest msg from the queue

          if checkMsgType(mQ) then
            Some(Iterator(List(mQidx)) -> Set(((checkMsgType, extractField), 0)))
          else None
        }
      val predicate: Expr[Map[String, Any] => Boolean] =
        generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
      val rhs: Expr[(Map[String, Any], ActorRef[M]) => T] =
        generateRhs[M, T](_rhs, inners, selfRef).asExprOf[(Map[String, Any], ActorRef[M]) => T]
      val size = 1

      val partialExtract = '{ (m: Tuple2[M, Int], mTree: MatchingTree[M]) =>
        val _extractors  = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
        val checkMsgType = _extractors.head._1
        val extractField = _extractors.head._2
        val (mQ, mQidx)  = m // Take the newest msg from the queue
        if checkMsgType(mQ) then
          Some(
            MatchingTree(
              mTree.nodeMapping + (List(mQidx) -> Set(((checkMsgType, extractField), 0)))
            )
          )
        else Some(mTree)
      }

      '{
        JoinPattern(
          $extract,
          $predicate,
          $rhs,
          ${ Expr(size) },
          $partialExtract
        )
      }

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
    _rhs: quotes.reflect.Term,
    self: String
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  val typesData = getTypesData(dataType)
  val extractors: List[(Expr[String], Expr[M => Boolean], Expr[M => Map[String, Any]])] =
    typesData.map { (outer, inners) =>
      val extractor = generateExtractor(outer, inners.map(_._1))

      outer.asType match
        case '[ot] =>
          (
            Expr(TypeTree.of[ot].symbol.name),
            '{ (m: M) => m.isInstanceOf[ot] },
            '{ (m: M) =>
              ${ extractor.asExprOf[ot => Map[String, Any]] }(m.asInstanceOf[ot])
            }
          )
    }.toList

  val (outers, inners) = (typesData.map(_._1), typesData.map(_._2).flatten)

  // val t = outers.map(_.classSymbol.get.getClass())
  // println(s"${t}")
  val extract: Expr[
    List[M] => Option[(Iterator[List[Int]], Set[((M => Boolean, M => Map[String, Any]), Int)])]
  ] =
    '{ (m: List[M]) =>
      val messages    = m.zipWithIndex
      val _extractors = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
      val msgPatterns = _extractors.zipWithIndex
      val patternSize = msgPatterns.size

      def countOccurences(typeNames: List[String]) =
        typeNames
          .map(t1 => (t1, typeNames.count(t2 => t1 equals t2)))
          .toMap

      val typeNamesInMsgs =
        messages.map((msg, idx) => (msg.getClass().getSimpleName(), idx))

      val patternInfo: Set[((M => Boolean, M => Map[String, Any]), Int)] =
        msgPatterns.map(msgPattern => ((msgPattern._1._2, msgPattern._1._3), msgPattern._2)).toSet

      val typesInPattern = countOccurences(msgPatterns.map(_._1._1))

      // println(s"typesInPattern: ${typesInPattern}")
      // println(s"typeNamesInMsgs: ${countOccurences(typeNamesInMsgs.map(_._1))}")
      def generateValidMsgCombs(messagesInQ: List[(String, Int)]): Iterator[List[Int]] =
        if messagesInQ.isEmpty then Iterator.empty[List[Int]]
        else
          val validCombs = messagesInQ
            .combinations(
              patternSize
            ) // Create all combinations of pattern size from the current messages in the mailbox
            .filter(comb =>
              val combOc = countOccurences(comb.map(_._1))
              // Filter the combinations that have the same pattern type composition as the composite pattern definition
              // println(
              //   s"Q ${combOc} -- P ${msgPatternOccurences} -- ${combOc == msgPatternOccurences}"
              // )
              /*
                P = A() & A() & B() & C()       ----          M = A(), A(), B(), C()

                PCount =
                  { class A -> 2,
                    class B -> 1,
                    class C -> 1 }

                MCount =
                  { class A -> 2,
                    class B -> 1,
                    class C -> 1 }

                PCount == MCount
               */
              combOc == typesInPattern
            )
            .map(_.map(_._2)) // Keep only the indicies. _._1 is the string name of a msg type
          validCombs

      val candidateMatches = generateValidMsgCombs(typeNamesInMsgs)

      if candidateMatches.isEmpty then None
      else Some((candidateMatches, patternInfo))
    }

  val predicate: Expr[Map[String, Any] => Boolean] =
    generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
  val rhs: Expr[(Map[String, Any], ActorRef[M]) => T] =
    generateRhs[M, T](_rhs, inners, self).asExprOf[(Map[String, Any], ActorRef[M]) => T]
  val size = outers.size

  val partialExtract: Expr[
    (Tuple2[M, Int], MatchingTree[M]) => Option[MatchingTree[M]]
  ] =
    '{ (m: Tuple2[M, Int], mTree: MatchingTree[M]) =>
      val _extractors       = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }
      val msgTypesInPattern = _extractors.map(pat => (pat._2, pat._3)).zipWithIndex

      val (mQ, mQidx) = m // Take the newest msg from the queue

      val isMsgInPat = msgTypesInPattern.exists { msgPat =>
        val ((checkMsgType, _), _) = msgPat
        checkMsgType(mQ)
      }

      if isMsgInPat then
        val matches = msgTypesInPattern.filter { msgPat =>
          val ((checkMsgType, _), _) = msgPat
          checkMsgType(mQ)
        }.toSet
        // println(
        //   s"matches: ${matches.size} -- mQ: $mQ -- mQidx: $mQidx "
        // )
        val newNodeMapping = mTree.nodeMapping.foldLeft(NodeMapping[M]()) { (acc, mapping) =>
          val (node, currentFits) = mapping
          val newFitsIdxs         = matches.map(_._2).diff(currentFits.map(_._2))
          // println(
          //   s"newFitsIdxs: $newFitsIdxs -- currentFits: ${currentFits.size} -- mQ: $mQ -- mQidx: $mQidx"
          // )

          if newFitsIdxs.isEmpty then
            // println(s"node ${node.mkString(",")} -- mQidx: ${mQidx}")
            if node.size < msgTypesInPattern.size then
              acc + (node.appended(mQidx) -> currentFits) + (List(mQidx) -> matches) + mapping
            else acc + (List(mQidx)       -> matches) + mapping
          else
            val currentFitsIdxs = currentFits.map(_._2)
            val newMappingIdxs  = currentFitsIdxs.`+`(newFitsIdxs.head)
            val newMapping = newMappingIdxs.map { idx =>
              val ((checkMsgType, extractField), _) = msgTypesInPattern(idx)
              ((checkMsgType, extractField), idx)
            }
            acc + ((node.appended(mQidx)) -> newMapping) + (List(mQidx) -> matches)
        }
        Some(MatchingTree(newNodeMapping))
      else Some(mTree)
    }

  '{
    JoinPattern(
      $extract,
      $predicate,
      $rhs,
      ${ Expr(size) },
      $partialExtract
    )
  }

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

  val extract: Expr[
    List[M] => Option[(Iterator[List[Int]], Set[((M => Boolean, M => Map[String, Any]), Int)])]
  ] = '{ (m: List[M]) =>
    None
  }
  val predicate: Expr[Map[String, Any] => Boolean] =
    generateGuard(guard, List()).asExprOf[Map[String, Any] => Boolean]
  val rhs: Expr[(Map[String, Any], ActorRef[M]) => T] = '{ (_: Map[String, Any], _: ActorRef[M]) =>
    ${ _rhs.asExprOf[T] }
  }
  val size = 1

  val partialExtract = '{ (m: Tuple2[M, Int], mTree: MatchingTree[M]) => None }

  '{
    JoinPattern(
      $extract,
      $predicate,
      $rhs,
      ${ Expr(size) },
      $partialExtract
    )
  }

/** Creates a join-pattern from a `CaseDef`.
  *
  * @param case
  *   the source `CaseDef`.
  * @return
  *   a join-pattern expression.
  */
private def generateJoinPattern[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    `case`: quotes.reflect.CaseDef,
    selfRef: String
): Option[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  `case` match
    case CaseDef(pattern, guard, _rhs) =>
      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, patterns), _) =>
          fun match
            case Select(_, "unapply") =>
              Some(generateSingletonPattern[M, T](t, guard, _rhs, selfRef))
            case TypeApply(Select(_, "unapply"), _) =>
              Some(generateCompositePattern[M, T](patterns, guard, _rhs, selfRef))
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
    expr: Expr[(M, ActorRef[M]) => T]
)(using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  // report.info(
  //   f"Inspect: ${expr.asTerm.show(using Printer.TreeStructure)}"
  // )
  expr.asTerm match
    case Inlined(_, _, Block(_, Block(stmts, _))) =>
      stmts.head match
        case DefDef(_, List(TermParamClause(params)), _, Some(Block(_, Match(_, cases)))) =>
          // report.info(
          //   s"${Printer.TreeStructure.show(params(1))}  --- ${params(1).tpt.tpe.dealias}"
          // )
          val selfRef = params(1).name
          // params.map { param =>
          //   Printer.TreeStructure.show(param)
          // }

          cases.flatMap(`case` => generateJoinPattern[M, T](`case`, selfRef))
        // code
        case default =>
          errorTree("Unsupported code", default)
          List()
    // case Inlined(_, _, Block(stmts, _)) =>
    //   stmts.head match
    //     case DefDef(_, _, _, Some(Match(_, cases))) =>
    //       cases.flatMap(generateJoinPattern[M, T](_))
    //     // report.info(
    //     //   f"Inspect: ${expr.asTerm.show(using Printer.TreeStructure)}"
    //     // )
    //     case default =>
    //       errorTree("Unsupported code", default)
    //       List()
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
  SelectMatcher[M, T](algorithm, ${ Expr.ofList(getCases(expr)) })
}

// private def receiveCodegen_[M, T](
//     expr: Expr[PartialFunction[Any, T]]
// )(using tm: Type[M], tt: Type[T], quotes: Quotes): Expr[MatchingAlgorithm => Matcher[M, T]] = '{
//   (algorithm: MatchingAlgorithm) =>
//     SelectMatcher[M, T](
//       algorithm,
//       ${ Expr.ofList(getCases(expr.asInstanceOf[Expr[PartialFunction[M, T]]])) }
//     )
// }

/** Entry point of the `receive` macro.
  *
  * @param f
  *   the block to use as source of the pattern-matching code.
  * @return
  *   a compile-time closure that takes a MatchingAlgorithm type and returns a Matcher-object that
  *   performs pattern-matching on a message queue at runtime.
  */
inline def receive[M, T](inline f: (M, ActorRef[M]) => T): MatchingAlgorithm => Matcher[M, T] =
  ${ receiveCodegen('f) }

// inline def receive_[M, T](inline f: PartialFunction[Any, T]): MatchingAlgorithm => Matcher[M, T] =
//   ${ receiveCodegen('f) }
