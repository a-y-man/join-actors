package join_patterns

import java.util.concurrent.{LinkedTransferQueue => Queue}

import scala.quoted.{Expr, Type, Quotes}
import scala.collection.mutable.ListBuffer

case class JoinPattern[M, T](
  var test: List[M] => Boolean,
  var extract: List[M] => Map[String, Any],
  var guard: Map[String, Any] => Boolean,
  var rhs: Map[String, Any] => T,
)

// are TypeRepr.simplified really useful ?
def extractOuter(using quotes: Quotes)(ua: quotes.reflect.Unapply): quotes.reflect.TypeRepr =
  import quotes.reflect.*

  ua.fun match
    case sel @ Select(_, "unapply") => sel.signature match
      case Some(sig) =>
        val extractor = ua.fun.etaExpand(ua.symbol)

        extractor.tpe match
          case AppliedType(TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), "Function1"),
            trepr :: _) => trepr.dealias.simplified match
              case tp: TypeRef => return tp
              case default => errorTypeRepr("Unsupported TypeRepr", default)
          case default => errorTypeRepr("Unsupported extractor type", extractor.tpe)
      case None => error("Unsupported Select", sel)
    case default => error("Unsupported unapply function", ua.fun)

  TypeRepr.of[Nothing]

// are TypeRepr.simplified really useful ?
def extractInner(using quotes: Quotes)(t: quotes.reflect.Tree): (String, quotes.reflect.TypeRepr) =
  import quotes.reflect.*

  var inner: (String, quotes.reflect.TypeRepr) = ("", TypeRepr.of[Nothing])

  t match
    case Bind(n, typed @ Typed(_, TypeIdent(_))) => typed.tpt.tpe.dealias.simplified match
      case tp: TypeRef => inner = (n, typed.tpt.tpe.dealias.simplified)
      case default => error("Unsupported inner type", default)
    case default => error("Unsupported pattern", t)

  inner

def makeExtractor(using quotes: Quotes)(outerType: quotes.reflect.TypeRepr, varNames: List[String]):
  quotes.reflect.Block =
  import quotes.reflect.*
  import scala.quoted.Varargs

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List(""))(_ => List(outerType), _ => TypeRepr.of[Map[String, Any]]),
    rhsFn = (sym: Symbol, params: List[Tree]) =>
      val p0 = params.head.asInstanceOf[Ident]
      val isMemberName: (Symbol => Boolean) = (p: Symbol) =>
        p.name.head == '_' && p.name.tail.toIntOption.isDefined
      val memberSymbols: List[Symbol] = outerType.typeSymbol.methodMembers.filter(isMemberName(_))
        .sortBy(_.name)

      if varNames.length != memberSymbols.length then
        report.warning(f"Number of member names was ${varNames.length}, but number of member accessors was ${memberSymbols.length}")

      val args = varNames.zipWithIndex.map {
        (name, i) => Expr.ofTuple(Expr(name), Select(p0, memberSymbols(i)).asExprOf[Any])
      }

      ('{ Map[String, Any](${ Varargs[(String, Any)](args) }: _*) }).asTerm
  )

def generateGuard(using quotes: Quotes)
                    (guard: Option[quotes.reflect.Term],
                     inners: Map[String, quotes.reflect.TypeRepr]): quotes.reflect.Block =
  import quotes.reflect.*

  val _transform = new TreeMap {
    override def transformTerm(term: Term)(owner: Symbol): Term = super.transformTerm(term)(owner)
  }

  var _rhsFn = (sym: Symbol, params: List[Tree]) =>
    _transform.transformTerm(('{true}).asExprOf[Boolean].asTerm.changeOwner(sym))(sym)

  guard match
    case Some(apply @ Apply(Select(_, "apply"), _)) =>
      if inners.isEmpty then
        _rhsFn = (sym: Symbol, params: List[Tree]) =>
            _transform.transformTerm(apply.changeOwner(sym))(sym)
      else
        _rhsFn = (sym: Symbol, params: List[Tree]) =>
          val p0 = params.head.asInstanceOf[Ident]

          val transform = new TreeMap {
            override def transformTerm(term: Term)(owner: Symbol): Term = term match
              case Ident(n) if inners.contains(n) =>
                val inner = '{${p0.asExprOf[Map[String, Any]]}(${Expr(n)})}
                inners.get(n).get.asType match
                  case '[innerType] => ('{${inner}.asInstanceOf[innerType]}).asTerm
              case x => super.transformTerm(x)(owner)
          }

          transform.transformTerm(apply.changeOwner(sym))(sym)

    case None => ()
    case Some(default) => error("Unsupported guard", default, Some(default.pos))

  Lambda(
    owner = Symbol.spliceOwner,
    tpe =  MethodType(List("_"))
                     (_ => List(TypeRepr.of[Map[String, Any]]), _ => TypeRepr.of[Boolean]),
    rhsFn = _rhsFn
  )

def generateRhs[T](using quotes: Quotes, tt: Type[T])
                 (rhs: quotes.reflect.Term, inners: Map[String, quotes.reflect.TypeRepr]):
                   quotes.reflect.Block =
  import quotes.reflect.*

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("_"))(_ => List(TypeRepr.of[Map[String, Any]]), _ => TypeRepr.of[T]),
    rhsFn = (sym: Symbol, params: List[Tree]) =>
      val p0 = params.head.asInstanceOf[Ident]

      val transform = new TreeMap {
        override def transformTerm(term: Term)(owner: Symbol): Term = term match
          case Ident(n) if inners.contains(n) =>
            val inner = '{${p0.asExprOf[Map[String, Any]]}(${Expr(n)})}

            inners.get(n).get.asType match
              case '[innerType] => ('{${inner}.asInstanceOf[innerType]}).asTerm
          case x => super.transformTerm(x)(owner)
      }

      transform.transformTerm(rhs.changeOwner(sym))(sym)
  )

def generate[M, T](using quotes: Quotes, tm: Type[M], tt: Type[T])(_case: quotes.reflect.CaseDef):
  Expr[JoinPattern[M, T]] =
  import quotes.reflect.*
  import scala.quoted.Varargs

  var test: Expr[List[M] => Boolean] = '{(_: List[M]) => true}
  var extract: Expr[List[M] => Map[String, Any]] = '{(_: List[M]) => Map()}
  var predicate: Expr[Map[String, Any] => Boolean] = null
  var rhs: Expr[Map[String, Any] => T] = null

  _case match
    case CaseDef(pattern, guard, _rhs) =>
      //report.warning(_rhs.show(using Printer.TreeStructure))

      pattern match
        case TypedOrTest(tree, tpd) =>
          tree match
            // A(*)
            case ua @ Unapply(sel @ Select(Ident(_), "unapply"), Nil, patterns) =>
              val outerType = extractOuter(ua) // replace by tpd ?
              val inners = patterns.map(extractInner(_)).toMap

              outerType.asType match
                case '[ot] =>
                  test = '{ (m: List[M]) => m.exists(_.isInstanceOf[ot]) }

                  if !patterns.isEmpty then
                    val extractor = makeExtractor(outerType, inners.map(_._1).toList)

                    extract = '{
                      (m: List[M]) =>
                        ${extractor.asExprOf[ot => Map[String, Any]]}(m(0).asInstanceOf[ot])
                    }

              predicate = generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
              rhs = generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
            // (A, B, C ...)
            case Unapply(TypeApply(Select(Ident(_), "unapply"), _), Nil, patterns) =>
              var outerTypes = patterns.map {
                case TypedOrTest(_, tt: TypeTree) => tt.tpe.dealias.simplified match
                  case tp @ TypeRef(_, name) => (tp.classSymbol.get.fullName, tp)
              }

              /*
              test = '{
                (m: List[M]) =>
                  outerTypes.forall {
                    (outerType: TypeRef) => outerType.asType match
                      case '[ot] => m.exists(_.isInstanceOf[ot])
                  }
              }
              */

              val outers = Expr.ofList(outerTypes.map(p => Expr(p._1)))
              /*
                make into outers List[Expr[(m: M) => Boolean]]
                iterate through messages
                  if outers.exists(message)
                    remove outer
                    if outers empty
                      return true
                return false
                val _outerTypes = Expr.ofList(outerTypes.map(p => Expr(p._2.asType)))
              */

              test = '{
                (m: List[M]) => m.map(_.getClass.getName).sorted == (${outers}).sorted
              }

              // extractor if inners

              val inners: Map[String, TypeRepr] = Map()

              predicate = generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
              rhs = generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
            case default => errorTree("Unsupported test", default)
        case w: Wildcard =>
          report.info("Wildcards should be defined last", w.asExpr)

          predicate = generateGuard(guard, Map()).asExprOf[Map[String, Any] => Boolean]
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

// shamelessly stolen from http://kennemersoft.nl/?page_id=96
def nonRepeatingComb[T](elems: List[T], genSize: Int, f: List[T] => Unit): Unit = {
    def nonRepeatingCombRec(elems: List[T], depth: Int, partResult: List[T]): Unit = {
        if (elems.size == depth) f(elems.reverse ::: partResult)
        else {
            if (!elems.isEmpty) {
                nonRepeatingCombRec(elems.tail, depth, partResult)
                if (depth > 0) nonRepeatingCombRec(elems.tail, depth - 1, elems.head :: partResult)
            }
        }
    }
    if (genSize < 0)
      throw new IllegalArgumentException("Negative generation sizes not allowed in nonRepeatingComb...")
    if (genSize > elems.size)
      throw new IllegalArgumentException("Generation sizes over elems.size not allowed in nonRepeatingComb...")
    nonRepeatingCombRec(elems.reverse, genSize, Nil)
}

// Generate the code returned by the receive macro
def receiveCodegen[M, T](expr: Expr[M => T])
                        (using tm: Type[M], tt: Type[T], quotes: Quotes): Expr[Queue[M] => T] =
  import quotes.reflect.*
  import collection.convert.ImplicitConversions._

  val genCode = '{
    (q: Queue[M]) =>
      val matchTable = ${ Expr.ofList(getCases(expr)) }
      var matched: Option[T] = None

      /*
      patterns should be declared by decreasing number of messages
      patterns should also retain the number of unmatched messages ?
      PriorityQueue[(n_unmatched, n_total, Patterns)]
      collection.mutable.PriorityQueue
      */

      //strategy function
      while (matched.isEmpty)
        val messages: ListBuffer[M] = ListBuffer(q.take())
        q.drainTo(messages)
        val messageSets: ListBuffer[List[M]] = ListBuffer()

        for i <- messages.size to 1 by -1 do
          nonRepeatingComb(messages.toList, i, messageSets.addOne)

        messageSets.toList.find {
          (msgSet: List[M]) =>
            println(msgSet)
            matchTable.exists {
              pattern =>
                println("pattern")
                if pattern.test(msgSet) then
                  val inners = pattern.extract(msgSet)
                  println("test")

                  if pattern.guard(inners) then
                    println("guard")
                    matched = Some(pattern.rhs(inners))
                    true
                  else false
                else false
            }
        } match
          case None => messages.foreach(e => q.put(e))
          case Some(set: List[M]) =>
            messages --= set
            messages.foreach(e => q.put(e))

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