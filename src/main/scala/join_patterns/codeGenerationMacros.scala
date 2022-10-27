package join_patterns

import java.util.concurrent.LinkedTransferQueue as Queue
import scala.quoted.{Expr, Quotes, Type, Varargs}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as mutMap
import scala.language.postfixOps

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
    // add support for Wildcard !!! : (_)
    // add support for Bind(String, WildCard) : (name: _)
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
                case Ident(n)
                  if inners.exists(_._1 == n) =>
                    val inner = '{ (${ p0.asExprOf[Map[String, Any]] })(${ Expr(n) }) }
                    report.info(s"generateGuard:transformTerm ===> ${inner.show}")

                    inners.find(_._1 == n).get._2.asType match
                      case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
                case x =>
                  // report.info(s"generateGuard:transformTerm ---> ${x}")

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

/** Creates a join-pattern from a `CaseDef`.
  *
  * @param case
  *   the source `CaseDef`.
  * @return
  *   a join-pattern expression.
  */
private def generate[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(
    `case`: quotes.reflect.CaseDef
): Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  var extract: Expr[List[M] => (List[M], Map[String, Any])] = '{ (_: List[M]) => (List(), Map()) }
  var predicate: Expr[Map[String, Any] => Boolean]          = '{ (_: Map[String, Any]) => true }
  var rhs: Expr[Map[String, Any] => T] = '{ (_: Map[String, Any]) => Object().asInstanceOf[T] }
  var size                             = 1

  `case` match
    case CaseDef(pattern, guard, _rhs) =>
      // report.warning(_rhs.show(using Printer.TreeStructure))

      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, patterns), _) =>
          val (outers, inners): (List[TypeRepr], List[(String, TypeRepr)]) = fun match
            // A(*)
            case Select(_, "unapply") =>
              val typesData       = getTypesData(List(t))
              val (outer, inners) = typesData.head

              outer.asType match
                case '[ot] =>
                  val extractor = generateExtractor(outer, inners.map(_._1))
                    .asExprOf[ot => Map[String, Any]]

                  extract = '{ (m: List[M]) =>
                    m.find(_.isInstanceOf[ot]) match
                      case None => (List(), Map())
                      case Some(message) =>
                        (List(message), $extractor(message.asInstanceOf[ot]))
                  }
              (List(outer), inners)
            // (A, B, C ...)
            case TypeApply(Select(_, "unapply"), _) =>
              val typesData = getTypesData(patterns)
              val extractors: List[(Expr[M => Boolean], Expr[M => Map[String, Any]])] =
                typesData.map { (outer, inners) =>
                  // try to replace by isInstanceOf[A | B | ...]
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

              extract = '{ (m: List[M]) =>
                val messages                    = ListBuffer.from(m)
                val matched: ListBuffer[M]      = ListBuffer()
                val fields: mutMap[String, Any] = mutMap()
                val _extractors                 = ${ Expr.ofList(extractors.map(Expr.ofTuple(_))) }

                if messages.size >= _extractors.size then
                  for
                    // Cannot use "(typecheck, extractor) <- _extractors"
                    // unecessary typecheck creates unreachable "case"
                    extractor <- _extractors
                    if matched.size < _extractors.size
                  do
                    messages.find(extractor._1) match
                      case Some(matchedMessage) =>
                        matched.addOne(matchedMessage)
                        messages.subtractOne(matchedMessage)
                        fields.addAll(extractor._2(matchedMessage))
                      case None => ()

                if matched.size == _extractors.size then (matched.toList, fields.toMap)
                else (List(), Map())
              }
              (typesData.map(_._1), typesData.map(_._2).flatten)
            case default =>
              errorTree("Unsupported tree", default)
              (List(), List())

          predicate = generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
          rhs = generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
          size = outers.size
        case w: Wildcard =>
          report.info("Wildcards should be defined last", w.asExpr)

          extract = '{ (m: List[M]) => (m, Map()) }
          predicate = generateGuard(guard, List()).asExprOf[Map[String, Any] => Boolean]
          rhs = '{ (_: Map[String, Any]) => ${ _rhs.asExprOf[T] } }
        case default => errorTree("Unsupported case pattern", default)

  '{ JoinPattern($extract, $predicate, $rhs, ${ Expr(size) }) }

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
          val code = cases.map { generate[M, T](_) }

          // for exp <- code do _println(exp)

          // report.info(
          //   f"Generated code: ${Expr.ofList(code).asTerm.show(using Printer.TreeAnsiCode)}"
          // )
          code
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
)(using tm: Type[M], tt: Type[T], quotes: Quotes) = '{
  Matcher[M, T](${ Expr.ofList(getCases(expr)) })
}
private def receiveCodegenOrd[M, T](expr: Expr[M => T], strat: Expr[Ordering[JoinPattern[M, T]]])(
    using
    tm: Type[M],
    tt: Type[T],
    quotes: Quotes
) = '{
  Matcher[M, T]((${ Expr.ofList(getCases(expr)) }).sorted($strat))
}

/** Entry point of the `receive` macro.
  *
  * @param f
  *   the block to use as source of the pattern-matching code.
  * @return
  *   a comptime function performing pattern-matching on a message queue at runtime.
  */
inline def receive[M, T](inline f: M => T): Matcher[M, T] = ${ receiveCodegen('f) }
inline def receiveOrd[M, T](inline s: Ordering[JoinPattern[M, T]])(
    inline f: M => T
): Matcher[M, T] = ${ receiveCodegenOrd('f, 's) }

/*
receive(e) {
	(A, B, C) => println(""), // local copies, used data is marked for `this` but not consumed (so others can use it)
	(A & B & C) => println(""), // all present at the same time in LinkedTransferQueue[M]
	(B -> D) => println(""), // sequential
	(A -> (D, B)) => println(""),
	A | B => println(""), // disjunction
	~Debug => println(""), // not
}
 */
