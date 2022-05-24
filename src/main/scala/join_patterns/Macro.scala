package join_patterns

import java.util.concurrent.{LinkedTransferQueue => Queue}

import scala.quoted.{Expr, Type, Quotes}
import scala.collection.mutable.ListBuffer

case class JoinPattern[M, T](
  var test: List[M] => Boolean, // List[M] => Map[M, Map[String, Any]] list of matched
  var extract: List[M] => Map[String, Any],
  var guard: Map[String, Any] => Boolean,
  var rhs: Map[String, Any] => T,
  val size: Int,
)

// are TypeRepr.simplified really useful ?
def extractInner(using quotes: Quotes)(t: quotes.reflect.Tree): (String, quotes.reflect.TypeRepr) =
  import quotes.reflect.*

  var inner: (String, quotes.reflect.TypeRepr) = ("", TypeRepr.of[Nothing])

  t match
    case Bind(n, typed @ Typed(_, TypeIdent(_))) => typed.tpt.tpe.dealias.simplified match
      case tp: TypeRef => inner = (n, typed.tpt.tpe.dealias.simplified)
      case default => error("Unsupported inner type", default)
    case default => errorTree("Unsupported pattern", t)

  inner

// add support for wildcard inner name, A(_: Int) -> no need to extract !
def getTypesData(using quotes: Quotes)(patterns: List[quotes.reflect.Tree]):
  List[(quotes.reflect.TypeRepr, Map[String, quotes.reflect.TypeRepr])] =
  import quotes.reflect.*

  patterns.map {
    case TypedOrTest(Unapply(Select(_, "unapply"), _, binds), tt: TypeTree) =>
      tt.tpe.dealias.simplified match
        case tp: TypeRef => tp -> binds.map(extractInner(_)).toMap
    case default =>
      errorTree("Unsupported pattern", default)
      TypeRepr.of[Nothing] -> Map()
  }

def generateExtractor(using quotes: Quotes)
                     (outerType: quotes.reflect.TypeRepr, varNames: List[String]):
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

  var test: Expr[List[M] => Boolean] = '{(_: List[M]) => true}
  var extract: Expr[List[M] => Map[String, Any]] = '{(_: List[M]) => Map()}
  var predicate: Expr[Map[String, Any] => Boolean] = '{(_: Map[String, Any]) => true}
  var rhs: Expr[Map[String, Any] => T] = null
  var size = 1

  _case match
    case CaseDef(pattern, guard, _rhs) =>
      //report.warning(_rhs.show(using Printer.TreeStructure))

      pattern match
        case t @ TypedOrTest(Unapply(fun, Nil, patterns), tt) =>
          fun match
            // A(*)
            case Select(_, "unapply") =>
              val (outer, inners) = getTypesData(List(t)).head

              outer.asType match
                case '[ot] =>
                  test = '{ (m: List[M]) => m.size == 1 && m.exists(_.isInstanceOf[ot]) }

                  if !patterns.isEmpty then
                    val extractor = generateExtractor(outer, inners.keys.toList)

                    extract = '{
                      (m: List[M]) =>
                        ${extractor.asExprOf[ot => Map[String, Any]]}(m(0).asInstanceOf[ot])
                    }

              predicate = generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
              rhs = generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
            // (A, B, C ...)
            case TypeApply(Select(_, "unapply"), _) =>
              val typesData = getTypesData(patterns)

              val extractors: List[(Expr[M => Boolean], Expr[M => Map[String, Any]])] =
                typesData.map {
                  (outer, inners) =>
                    val extractor = generateExtractor(outer, inners.keys.toList)

                    outer.asType match
                      case '[ot] => (
                        '{(m: M) => m.isInstanceOf[ot]},
                        '{
                          (m: M) =>
                            ${extractor.asExprOf[ot => Map[String, Any]]}(m.asInstanceOf[ot])
                        }
                      )
                }.toList

              test = '{
                (m: List[M]) =>
                  val outerTypeChecks = ${Expr.ofList(extractors.map(_._1))}

                  m.size == outerTypeChecks.size && {
                    val typechecks = ListBuffer.from(outerTypeChecks)

                    m.exists(
                      (message: M) => typechecks.find(_(message)) match
                        case Some(typecheck) => typechecks.subtractOne(typecheck).isEmpty
                        case None => false
                    )
                  }
              }

              val inners: Map[String, TypeRepr] = typesData.map(_._2).flatten.toMap

              if !inners.isEmpty then
                extract = '{
                  (m: List[M]) =>
                    val messages = ListBuffer.from(m)

                    ${Expr.ofList(extractors.map(Expr.ofTuple(_)))}.map {
                      (typecheck, extractor) =>
                        val matchedMessage = messages.find(typecheck).get
                        messages.subtractOne(matchedMessage)
                        extractor(matchedMessage)
                    }.flatten.toMap
                }

              predicate = generateGuard(guard, inners).asExprOf[Map[String, Any] => Boolean]
              rhs = generateRhs[T](_rhs, inners).asExprOf[Map[String, Any] => T]
              size = typesData.size
            case default => errorTree("Unsupported tree", default)
        case w: Wildcard =>
          report.info("Wildcards should be defined last", w.asExpr)

          predicate = generateGuard(guard, Map()).asExprOf[Map[String, Any] => Boolean]
          rhs = '{(_: Map[String, Any]) => ${_rhs.asExprOf[T]}}
        case default => errorTree("Unsupported case pattern", default)

  '{JoinPattern($test, $extract, $predicate, $rhs, ${Expr(size)})}

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
      val matchTable = (${ Expr.ofList(getCases(expr)) }).sortBy(_.size).reverse
      val patternSizes = matchTable.map(_.size).toSet
      var matched: Option[T] = None

      /*
      patterns should be declared by decreasing number of messages
      patterns should also retain the number of unmatched messages ?
      PriorityQueue[(n_unmatched, n_total, Pattern)]
      collection.mutable.PriorityQueue
      */

      //strategy function
      while (matched.isEmpty)
        val messages: ListBuffer[M] = ListBuffer(q.take())
        q.drainTo(messages)
        val messageSets: ListBuffer[List[M]] = ListBuffer()

        patternSizes.dropWhile(_ > messages.size).foreach(
          nonRepeatingComb(messages.toList, _, messageSets.addOne)
        )
        //patternSizes.foreach(repeatingComb(messages.toList, _, messageSets.addOne))

        messageSets.toList.find {
          (msgSet: List[M]) =>
            matchTable.exists {
              pattern =>
                if pattern.test(msgSet) then
                  val inners = pattern.extract(msgSet)

                  if pattern.guard(inners) then
                    matched = Some(pattern.rhs(inners))
                    true
                  else false
                else false
            }
        } match
          case None => messages.foreach(q.put)
          case Some(set: List[M]) => messages.subtractAll(set).foreach(q.put)

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