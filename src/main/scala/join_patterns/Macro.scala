package join_patterns

import java.util.concurrent.{LinkedTransferQueue => Queue}

import scala.quoted.{Expr, Type, Quotes}

case class JoinPattern[M, T](
  var test: List[M] => Boolean,
  var extract: List[M] => Map[String, Any],
  var guard: Map[String, Any] => Boolean,
  var rhs: Map[String, Any] => T,
)

// change to ((String, quotes.reflect.TypeRepr), Map[String, quotes.reflect.TypeRepr]) ?
def extractClassNames(using quotes: Quotes)(ua: quotes.reflect.Unapply): (String, String) =
  import quotes.reflect.*

  ua.fun match
    case sel @ Select(_, "unapply") => sel.signature match
      case Some(sig) =>
        val extractor = ua.fun.etaExpand(ua.symbol)

        extractor.tpe match
          case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Function1"),
            trepr :: _) => trepr.dealias.simplified match
              case tp: TypeRef => return (sig.resultSig, tp.classSymbol.get.fullName)
              case default => errorTypeRepr("Unsupported TypeRepr", default)
          case default => errorTypeRepr("Unsupported extractor type", extractor.tpe)
      case None => error("Unsupported Select", sel)
    case default => error("Unsupported unapply function", ua.fun)

  ("", "")

def makeExtractor(using quotes: Quotes)
                 (outerType: quotes.reflect.TypeRepr, varNames: List[String]):
                  quotes.reflect.Block =
  import quotes.reflect.*
  import scala.quoted.Varargs

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("m"))(_ => List(outerType), _ => TypeRepr.of[Map[String, Any]]),
    rhsFn = (sym: Symbol, params: List[Tree]) =>
      val p0 = params.head.asInstanceOf[Ident]
      val isMemberName: (Symbol => Boolean) = (p: Symbol) =>
        p.name.head == '_' && p.name.tail.toIntOption.isDefined
      val memberSymbols: List[Symbol] = outerType.typeSymbol.methodMembers.filter(isMemberName(_))

      if varNames.length != memberSymbols.length then
        report.warning(f"Number of member names was ${varNames.length}, but number of member accessors was ${memberSymbols.length}")

      val args = varNames.zipWithIndex.map {
        (name, i) => Expr.ofTuple(Expr(name), Select(p0, memberSymbols(i)).asExprOf[Any])
      }

      ('{ Map[String, Any](${ Varargs[(String, Any)](args) }: _*) }).asTerm
  )

def generateGuard[T](using quotes: Quotes, tt: Type[T])
                    (guard: Option[quotes.reflect.Term],
                    inners: Map[String, quotes.reflect.TypeRepr]): quotes.reflect.Block =
  import quotes.reflect.*

  val _transform = new TreeMap {
    override def transformTerm(term: Term)(owner: Symbol): Term =
      super.transformTerm(term)(owner)
  }

  var _rhsFn = (sym: Symbol, params: List[Tree]) =>
    _transform.transformTerm(('{true}).asExprOf[Boolean].asTerm.changeOwner(sym))(sym)

  guard match
    case Some(apply @ Apply(Select(_, "apply"), _)) =>
      if inners.isEmpty then
        _rhsFn = (sym: Symbol, params: List[Tree]) =>
            _transform.transformTerm(apply.changeOwner(sym))(sym)
      else
        // only considers last inner for now, all previous overwritten
        inners.foreach {
          (name, _type) =>
            _rhsFn = (sym: Symbol, params: List[Tree]) =>
              val p0 = params.head.asInstanceOf[Ident]

              val transform = new TreeMap {
                override def transformTerm(term: Term)(owner: Symbol): Term = term match
                  case Ident(n) if (n == name) =>
                    val inner = '{${p0.asExprOf[Map[String, Any]]}(${Expr(name)})}
                    _type.asType match
                      case '[innerType] => ('{${inner}.asInstanceOf[innerType]}).asTerm
                  case x => super.transformTerm(x)(owner)
              }

              transform.transformTerm(apply.changeOwner(sym))(sym)
        }
    case None => ()
    case Some(default) => error("Unsupported guard", default, guard match
      case Some(guard) => Some(guard.pos)
      case _ => None
      )

  Lambda(
    owner = Symbol.spliceOwner,
    tpe =  MethodType(List("inners")) // <-- ?
                     (_ => List(TypeRepr.of[Map[String, Any]]), _ => TypeRepr.of[Boolean]),
    rhsFn = _rhsFn
  )

def makeNewRhs[T](using quotes: Quotes, tt: Type[T])
                 (name: String, _type: quotes.reflect.TypeRepr,
                  rhs: quotes.reflect.Term): quotes.reflect.Block =
  import quotes.reflect.*

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("inners"))(_ => List(TypeRepr.of[Map[String, Any]]), _ => TypeRepr.of[T]),
    rhsFn = (sym: Symbol, params: List[Tree]) =>
      val p0 = params.head.asInstanceOf[Ident]

      val transform = new TreeMap {
        override def transformTerm(term: Term)(owner: Symbol): Term = term match
          case Ident(n) if (n == name) =>
            val inner = '{${p0.asExprOf[Map[String, Any]]}(${Expr(name)})}
            _type.asType match
              case '[innerType] => ('{${inner}.asInstanceOf[innerType]}).asTerm
          case x => super.transformTerm(x)(owner)
      }

      transform.transformTerm(rhs.changeOwner(sym))(sym)
  )

def generate[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(_case: quotes.reflect.CaseDef):
  Expr[JoinPattern[M, T]] =
  import quotes.reflect.*

  var test: Expr[List[M] => Boolean] = '{(m: List[M]) => true}
  var extract: Expr[List[M] => Map[String, Any]] = '{(_: List[M]) => Map()}
  var predicate: Expr[Map[String, Any] => Boolean] = null
  var rhs: Expr[Map[String, Any] => T] = null

  _case match
    case CaseDef(pattern, guard, _rhs) =>
      //report.warning(_rhs.show(using Printer.TreeStructure))

      pattern match
        case TypedOrTest(tree, tpd) =>
          tree match
            case ua @ Unapply(sel @ Select(_, "unapply"), Nil, patterns) =>
              val classNames = extractClassNames(ua)

              patterns match
                // A()
                case Nil =>
                  if (classNames._1 != "scala.Boolean") then
                    errorSig("Unsupported Signature", sel.signature.get)

                  test = '{
                    (m: List[M]) => m.find(_.getClass.getName == ${Expr(classNames._2)}).isDefined
                  }
                  predicate = generateGuard[T](guard, Map()).asExprOf[Map[String, Any] => Boolean]
                  rhs = '{ (inners: Map[String, Any]) => ${_rhs.asExprOf[T]} }
                // A(.)
                case List(bind @ Bind(varName, Typed(_, varType: TypeIdent))) =>
                  val extractor = makeExtractor(tpd.tpe, List(varName))
                  val _guard = generateGuard[T](guard, Map(varName -> varType.tpe))
                  val newRhs = makeNewRhs[T](varName, varType.tpe, _rhs)

                  (tpd.tpe.asType, varType.tpe.asType) match
                    case ('[ot], '[it]) =>
                      test = '{
                        (m: List[M]) =>
                          m.find(_.getClass.getName == ${Expr(classNames._2)}).isDefined
                      }
                      extract = '{
                        (m: List[M]) =>
                          ${extractor.asExprOf[ot => Map[String, Any]]}(m(0).asInstanceOf[ot])
                      }
                      predicate = _guard.asExprOf[Map[String, Any] => Boolean]
                      rhs = newRhs.asExprOf[Map[String, Any] => T]
                    case default => error("Unsupported type", default)
                // A(...)
                case binds @ List(Bind(name0, Typed(_, type0: Ident)), Bind(name1, Typed(_, type1: Ident))) =>
                  //val inners: List[(String, TypeIdent)] = extractInners()
                  val _guard = generateGuard[T](guard, Map())
                case default => error("Unsupported patterns", default)
            /*
            case Unapply(TypeApply(Select(Ident(_Tuple), "unapply"), args), Nil, classes) =>
              // (A, B, ...)
              // args : List(Inferred(), Inferred())
              classes.map {
                case TypedOrTest(Unapply(Select(Ident(typename), "unapply"), Nil, Nil), Inferred()) => ()
                case default => ()
              }

              val typenames = args.map {
                t => t.tpe.dealias.simplified.classSymbol.get.fullName
              }

              val _guard = generateGuard[T](guard)

              return '{JoinPattern(
                (m: List[M]) => true,
                (m: List[M]) => None,
                ${ _guard.asExprOf[Any => Boolean] },
                (p: Any) => ${rhs.asExprOf[T]}
              )}
            */
            case default => errorTree("Unsupported test", default)
        case w: Wildcard =>
          report.info("Wildcards should be defined last", w.asExpr)

          predicate = generateGuard[T](guard, Map()).asExprOf[Map[String, Any] => Boolean]
          rhs = '{(inners: Map[String, Any]) => ${_rhs.asExprOf[T]}}
        case default => errorTree("Unsupported case pattern", default)

  '{JoinPattern($test, $extract, $predicate, $rhs)}

// Translate a series of match clause into a list of pairs, each one
// containing a test function to check whether a message has a certain type,
// and a closure (returning T) to execute if the test returns true
def getCases[M, T](expr: Expr[M => T])(using quotes: Quotes, tm: Type[M], tt: Type[T]):
  List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*

  expr.asTerm match
    case Inlined(_, _, Block(_, Block(stmts, _))) =>
      stmts(0) match
        case DefDef(_, _, _, Some(Block(_, Match(_, cases)))) =>
          cases.map { generate[M, T](_) }
        case default =>
          errorTree("Unsupported code", default)
          List()
    case default =>
      errorTree("Unsupported expression", default)
      List()

// Generate the code returned by the receive macro
def receiveCodegen[M, T](expr: Expr[M => T])
                        (using tm: Type[M], tt: Type[T], quotes: Quotes): Expr[Queue[M] => T] =
  import quotes.reflect.*
  import scala.util.control.Breaks.break

  val genCode = '{
    (q: Queue[M]) =>
      val matchTable = ${ Expr.ofList(getCases(expr)) }
      var matched: Option[T] = None

      //strategy function
      while (matched.isEmpty)
        val msg = q.take()
        val messages = msg :: Nil

        matchTable.find {
          pattern =>
            if pattern.test(messages) then
              val inners = pattern.extract(messages)

              if pattern.guard(inners) then
                matched = Some(pattern.rhs(inners))
                true
              else false
            else
              false
        }

      matched.get
  }

  report.info(f"Generated code: ${genCode.asTerm.show(using Printer.TreeAnsiCode)}")
  genCode

/** Entry point of the `receive` macro.
 *
 *  @param f the block to use as source of the pattern-matching code.
 *  @return a comptime function performing pattern-matching on a message queue at runtime.
 */
inline def receive[M, T](inline f: M => T): Queue[M] => T = ${ receiveCodegen('f) }

/*
 primitive / composite event

val p = (C c -> D d)
val p1[Ty] = Ty c

receive(e) {
	d: D => println(""),
	D(d0, d1, d2) => println(""),
	(A a, B b, C c) => println(""), // local copies, used data is marked for `this` but not consumed (so others can use it)
	(A a & B b & C c) => println(""), // all present at the same time in LinkedTransferQueue[M]
	(B b -> D d) => println(""), // sequential
	(A a -> (D d, A a)) => println(""),
	A a | B b => println(""), // disjunction
	p => println(""), // pattern variable
	p1 => println(""), // parameterized pattern variable
	~Debug dbg => println(""), // not
	_ => println(""), // wildcard type
	E _ => println(""), // wildcard var
}
*/