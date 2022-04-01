package join_patterns

import java.util.concurrent.{LinkedTransferQueue => Queue}

import scala.quoted.{Expr, Type, Quotes}

def extractClassName(using quotes: Quotes)(ua: quotes.reflect.Unapply): String =
  import quotes.reflect.*

  ua.fun match
    case sel @ Select(Ident(x), "unapply") => sel.signature match
      case Some(sig) =>
        if (sig.resultSig == "scala.Boolean")
          val extractor = ua.fun.etaExpand(ua.symbol)

          extractor.tpe match
            case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Function1"), trepr :: _) =>
              trepr.dealias.simplified match
                case tp: TypeRef => return tp.classSymbol.get.fullName
                case default =>
                  report.error(f"Unsupported TypeRepr: ${default.show(using Printer.TypeReprStructure)}")
            case default =>
              report.error(f"Unsupported extractor type: ${extractor.tpe.show(using Printer.TypeReprStructure)}")
        else
          report.error(f"Unsupported unapply return type: ${sig.resultSig}")
      case None => report.error(f"Unsupported Select: ${sel.show(using Printer.TreeStructure)}")
    case default =>
      report.error(f"Unsupported unapply function: ${ua.fun.show(using Printer.TreeStructure)}")

  return ""

def generate[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])
            (_case: quotes.reflect.CaseDef): Expr[(List[M] => Boolean, () => T)] =
  import quotes.reflect.*

  _case match
    case CaseDef(TypedOrTest(tree, tpd), guard, rhs) => tree match
      case ua @ Unapply(sel @ Select(Ident(x), "unapply"), Nil, Nil) =>
        val tpName = Expr(extractClassName(ua))

        '{(
          (m: List[M]) => m.find(_.getClass.getName == ${tpName}).isDefined,
          () => ${rhs.asExprOf[T]}
        )}
      case ua @ Unapply(_/*TupleN*/, Nil, pats) =>
        val classes: Expr[List[String]] = Expr(pats.map {
          case TypedOrTest(ua @ Unapply(sel @ Select(Ident(x), "unapply"), Nil, Nil), _) =>
            extractClassName(ua)
          case default =>
            report.error(f"Unsupported test: ${default.show(using Printer.TreeStructure)}")
            ""
        })

        '{(
          (m: List[M]) => ${classes}.forall(
            c_c => m.find(_.getClass.getName == c_c.getClass.getName).isDefined
          ),
          () => ${rhs.asExprOf[T]} )
        }
      case default =>
        report.error(f"Unsupported test: ${default.show(using Printer.TreeStructure)}")
        null
    case default =>
      report.error(f"Unsupported match clause: ${default.show(using Printer.TreeStructure)}")
      null


// Translate a series of match clause into a list of pairs, each one
// containing a test function to check whether a message has a certain type,
// and a closure (returning T) to execute if the test returns true
def getCases[M, T](expr: Expr[M => T])
                  (using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[(List[M] => Boolean, () => T)]] =
  import quotes.reflect.*

  expr.asTerm match
    case Inlined(_, _, Block(_, Block(stmts, _))) =>
      stmts(0) match
        case DefDef(_, _, _, Some(Block(_, Match(_, cases)))) =>
          cases.map {generate[M, T](_)}
        case default =>
          report.error(f"Unsupported code: ${default.show(using Printer.TreeStructure)}")
          List()
    case default =>
      report.error(f"Unsupported code: ${default.show(using Printer.TreeStructure)}")
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
        matchTable.find(_._1(msg :: Nil)) match
          case Some(m) => matched = Some(m._2())
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
inline def receive[M, T](inline f: M => T): Queue[M] => T =
  ${ receiveCodegen('f) }

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