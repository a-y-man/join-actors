package join_patterns

import java.util.concurrent.{LinkedTransferQueue => Queue}

import scala.quoted.{Expr, Type, Quotes}

def errorTree(using quotes: Quotes)(msg: String, token: quotes.reflect.Tree): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)

  token.symbol.pos match
    case Some(pos) => report.error(f"$msg: $t", pos)
    case None => report.error(f"$msg: $t")

def errorTypeRepr(using quotes: Quotes)(msg: String, token: quotes.reflect.TypeRepr): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TypeReprStructure)

  token.termSymbol.pos match
    case Some(pos) => report.error(f"$msg: $t", pos)
    case None => report.error(f"$msg: $t")

def errorSig(using quotes: Quotes)(msg: String, token: quotes.reflect.Signature): Unit =
  import quotes.reflect.*

  report.error(f"$msg: ${token.paramSigs} => ${token.resultSig}")

def error[T](using quotes: Quotes)
            (msg: String, token: T, pos: Option[quotes.reflect.Position] = None): Unit =
  import quotes.reflect.*

  val show: String = token match
    case s: String => s
    case _ => token.toString

  val _pos: Position = token match
    case _ if pos.isDefined => pos.get

  report.error(f"$msg: $show", _pos)

def extractClassName(using quotes: Quotes)(ua: quotes.reflect.Unapply): String =
  import quotes.reflect.*

  ua.fun match
    case sel @ Select(Ident(_), "unapply") => sel.signature match
      case Some(sig) =>
        if (sig.resultSig == "scala.Boolean")
          val extractor = ua.fun.etaExpand(ua.symbol)

          extractor.tpe match
            case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Function1"),
              trepr :: _) => trepr.dealias.simplified match
                case tp: TypeRef => return tp.classSymbol.get.fullName
                case default => errorTypeRepr("Unsupported TypeRepr", default)
            case default => errorTypeRepr("Unsupported extractor type", extractor.tpe)
        else errorSig("Unsupported Signature", sig)
      case None => error("Unsupported Select", sel)
    case default => error("Unsupported unapply function", ua.fun)

  ""

// REFACTOR
def generateGuard(using quotes: Quotes)
                 (guard: Option[quotes.reflect.Term],
                  variable: Option[(String, quotes.reflect.TypeIdent)] = None): quotes.reflect.Block =
  import quotes.reflect.*

  guard match
    case Some(apply: Apply) =>
      variable match
        case Some(name, _type) => _type.tpe.asType match
          case '[t] => ()//return apply.fun.etaExpand(apply.symbol).asExprOf[Function[t, Boolean]]
          return Lambda(
            owner = Symbol.spliceOwner,
            tpe = MethodType(List(name))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Boolean]),
            rhsFn = (sym: Symbol, params: List[Tree]) => {
              val p0 = params.head.asInstanceOf[Ident]

              val transform = new TreeMap {
                override def transformTerm(term: Term)(owner: Symbol): Term = {
                  val nt: Term = term match {
                    case Ident(n) if (n == name) => p0
                    case x => super.transformTerm(x)(owner)
                  }
                  nt
                }
              }
              transform.transformTerm(apply.changeOwner(sym))(sym)
            }
          )
        case None =>
          return Lambda(
            owner = Symbol.spliceOwner,
            tpe = MethodType(List("m"))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Boolean]),
            rhsFn = (sym: Symbol, params: List[Tree]) => {
              val transform = new TreeMap {
                override def transformTerm(term: Term)(owner: Symbol): Term =
                  super.transformTerm(term)(owner)
              }
              transform.transformTerm(apply.changeOwner(sym))(sym)
            }
          )
    case None => ()
    case Some(default) => error("Unsupported guard", default)

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("m"))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Boolean]),
    rhsFn = (sym: Symbol, params: List[Tree]) => {
      val transform = new TreeMap {
        override def transformTerm(term: Term)(owner: Symbol): Term =
          super.transformTerm(term)(owner)
      }
      transform.transformTerm(('{true}).asExprOf[Boolean].asTerm.changeOwner(sym))(sym)
    }
  )

def makeNewRhs[T](using quotes: Quotes, tt: Type[T])
                 (name: String, _type: quotes.reflect.TypeRepr, rhs: quotes.reflect.Term): quotes.reflect.Block =
  import quotes.reflect.*

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List(name))(_ => List(_type), _ => TypeRepr.of[T]),
    rhsFn = (sym: Symbol, params: List[Tree]) => {
      val p0 = params.head.asInstanceOf[Ident]

      val transform = new TreeMap {
        override def transformTerm(term: Term)(owner: Symbol): Term = {
          val nt: Term = term match {
            case Ident(n) if (n == name) => p0
            case x => super.transformTerm(x)(owner)
          }
          nt
        }
      }
      transform.transformTerm(rhs.changeOwner(sym))(sym)
    }
  )

def generate[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])
            (_case: quotes.reflect.CaseDef):
              Expr[(List[M] => Boolean, List[M] => Option[Any], Any => Boolean, () => T)] =
  import quotes.reflect.*

  _case match
    case CaseDef(pattern, guard, rhs) =>

      pattern match
        case TypedOrTest(tree, tpd) =>
          tree match
            case ua @ Unapply(sel @ Select(Ident(_), "unapply"), Nil, patterns) =>
              println(patterns)
              patterns match
                case Nil =>
                  val tpName = Expr(extractClassName(ua))
                  val _guard = generateGuard(guard)

                  return '{(
                    (m: List[M]) => m.find(_.getClass.getName == ${tpName}).isDefined,
                    (m: List[M]) => None,
                    (p: Any) => { val test = ${_guard.asExprOf[Any => Boolean] }; test(p)},
                    () => ${rhs.asExprOf[T]}
                  )}
                case List(bind @ Bind(varName, typed @ Typed(_, varType @ TypeIdent(_type)))) =>
                  //report.info(bind.show(using Printer.TreeStructure), bind.pos)

                  // REFACTOR
                  val _class = Expr(ua.fun match
                      case sel @ Select(Ident(_), "unapply") => sel.signature match
                        case Some(sig) => sig.resultSig
                        case None => ""
                      case _ => ""
                    )

                  val _guard = generateGuard(guard, Some(varName, varType))
                  val newRhs = makeNewRhs[T](varName, varType.tpe, rhs)

                  println(prettyPrint(_guard))

                  return '{(
                    (m: List[M]) => m.find(_.getClass.getName == ${_class}).isDefined,
                    (m: List[M]) => Some(${Expr(varName)}),
                    (p: Any) => { val test = ${_guard.asExprOf[Any => Boolean] }; test(p)},
                    () => { val test = ${ newRhs.asExprOf[Int => T] }; test(42) }
                  )}
                /*
                case List(_) =>
                  println("List(_)")
                  val classes = patterns.map {
                    case TypedOrTest(ua1 @ Unapply(sel @ Select(Ident(x), "unapply"), Nil, Nil), _) =>
                      extractClassName(ua1)
                    case w: Wildcard => w.name
                    case default =>
                      errorTree("Unsupported pattern", default)
                      ""
                  }
                  val length = Expr(classes.length)

                  return '{(
                    (m: List[M]) =>
                      m.length >= ${length} && ${Expr(classes)}.forall(
                        c_c => m.find(_.getClass.getName == c_c.getClass.getName).isDefined || c_c == "_"
                      ),
                    $_guard,
                    () => ${rhs.asExprOf[T]}
                  )}
                */
                case default => error("Unsupported patterns", default)
            // (A, B, ...)
            /*
            case Unapply(TypeApply(Select(Ident(_Tuple), "unapply"), args), Nil, classes) =>
              // args : List(Inferred(), Inferred())
              classes.map {
                case TypedOrTest(Unapply(Select(Ident(typename), "unapply"), Nil, Nil), Inferred()) => ()
                case default => ()
              }

              val typenames = args.map {
                t => t.tpe.dealias.simplified.classSymbol.get.fullName
              }

              return '{(
                (m: List[M]) => true,
                $_guard,
                () => ${rhs.asExprOf[T]}
              )}
            */
            case default => errorTree("Unsupported test", default)
        case Wildcard() =>
          val _guard = generateGuard(guard)

          return '{(
            (m: List[M]) => true,
            (m: List[M]) => None,
            (p: Any) => { val test = ${_guard.asExprOf[Any => Boolean] }; test(p)},
            () => ${rhs.asExprOf[T]}
          )}
        case default => errorTree("Unsupported case pattern", default)

  null

// Translate a series of match clause into a list of pairs, each one
// containing a test function to check whether a message has a certain type,
// and a closure (returning T) to execute if the test returns true
def getCases[M, T](expr: Expr[M => T])
                  (using quotes: Quotes, tm: Type[M], tt: Type[T]):
                    List[Expr[(List[M] => Boolean, List[M] => Option[Any], Any => Boolean, () => T)]] =
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

  val genCode = '{
    (q: Queue[M]) =>
      val matchTable = ${ Expr.ofList(getCases(expr)) }
      var matched: Option[T] = None

      while (matched.isEmpty)
        val msg = q.take()
        val messages = msg :: Nil

        matchTable.find(_._1(messages)) match
          case Some(m) => m._2(messages) match
            case Some(inners) => if m._3(inners) then matched = Some(m._4())
            case None => if m._3(None) then matched = Some(m._4())
          case _ => ()

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