package join_patterns.code_generation

import join_actors.actor.ActorRef
import join_patterns.types.*
import join_patterns.util.*

import scala.quoted.{Expr, Quotes, Type}

/** Extracts a variable binding's name and type representation from a pattern tree node.
  *
  * Handles: `name: Type`, `_: Type`, `name` (untyped wildcard), `_` (wildcard).
  *
  * @param t
  *   the tree, either a `Bind`, `Typed`, or `Wildcard`.
  * @return
  *   a tuple of (variable name, type representation). Wildcards use `"_"`.
  */
private[code_generation] def extractPayloads(using quotes: Quotes)(
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
      abortTreeWithHint(
        s"Unsupported payload binding",
        "Expected `name: Type`, `_: Type`, or `_` wildcard",
        t
      )

/** Whether `fun` is the `unapply` that the compiler generates for the case class matched by `tt`.
  *
  * A join pattern is destructured through the case class's own fields. A user-defined extractor
  * (`object Even { def unapply(n: N): Option[Int] = ... }`, or a hand-written `unapply` in the
  * companion) has different semantics that the macro would silently ignore.
  */
private[code_generation] def isCaseClassUnapply(using quotes: Quotes)(
    fun: quotes.reflect.Tree,
    tt: quotes.reflect.TypeTree
): Boolean =
  import quotes.reflect.*

  val cls = tt.tpe.dealias.simplified.typeSymbol
  cls.flags.is(Flags.Case) && fun.symbol.flags.is(Flags.Synthetic)

/** Rejects payload patterns that test the type of a field, like `case Msg(x: Int)` for a field
  * declared as `Any`.
  *
  * A type test would have to decide whether the message matches at all, but the matchers only
  * classify messages by their constructor. The test would be silently dropped and a message with
  * another payload type would match and then fail with a `ClassCastException`.
  */
private[code_generation] def checkNoPayloadTypeTests(using quotes: Quotes)(
    constructor: quotes.reflect.TypeRef,
    payloads: List[(quotes.reflect.Tree, (String, quotes.reflect.TypeRepr))]
): Unit =
  import quotes.reflect.*

  val fields = constructor.typeSymbol.caseFields
  for
    ((tree, (name, written)), i) <- payloads.zipWithIndex
    field <- fields.lift(i)
    declared = constructor.memberType(field).dealias.simplified
    if !(written.dealias.simplified =:= declared)
  do
    abortTreeWithHint(
      "Unsupported type test on a message field",
      s"`${if name == "_" then "_" else name}: ${written.show}` tests the type of a field that is declared as " +
        s"`${declared.show}`. Join patterns cannot test payload types; bind the field without a " +
        s"type, or with its declared type, and test it in the guard",
      tree
    )

/** Extracts constructor type and field binding data from a list of pattern trees.
  *
  * @param patterns
  *   the patterns, as a `List[Tree]` of `TypedOrTest` nodes.
  * @return
  *   a list of (constructor TypeRepr, List of (field name, field TypeRepr)) tuples.
  */
private[code_generation] def extractConstructorData(using quotes: Quotes)(
    patterns: List[quotes.reflect.Tree]
): List[(quotes.reflect.TypeRepr, List[(String, quotes.reflect.TypeRepr)])] =
  import quotes.reflect.*

  patterns.map {
    case TypedOrTest(Unapply(fun @ Select(s, "unapply"), _, binds), tt: TypeTree)
        if isCaseClassUnapply(fun, tt) =>
      tt.tpe.dealias.simplified match
        case tp: TypeRef =>
          val payloads = binds.map(extractPayloads(_))
          checkNoPayloadTypeTests(tp, binds.zip(payloads))
          tp -> payloads
        case other =>
          abortTreeWithHint(
            "Unsupported message constructor type",
            s"`${other.show}` is not a plain class type; generic message classes are not supported",
            tt
          )
    case TypedOrTest(Unapply(fun, _, _), _) =>
      abortTreeWithHint(
        "Unsupported extractor in a join pattern",
        "Only the pattern of a case class, like `MsgType(field1, field2)`, is supported. " +
          "A user-defined `unapply` is not applied, so it cannot be used here",
        fun
      )
    case default =>
      abortTreeWithHint(
        "Unsupported message constructor type",
        "Expected a case class pattern like `MsgType(field1, field2)`",
        default
      )
  }

/** Recursively extracts individual constructor patterns from nested `&:&` operator applications.
  *
  * Since `&:&` is left-associative, the right child is always a `TypedOrTest` leaf,
  * while the left child is either another `&:&` application or a `TypedOrTest` leaf.
  *
  * @param unapplyTree
  *   the `Unapply` node representing an `&:&` application.
  * @return
  *   a flat list of `TypedOrTest` pattern nodes.
  */
private[code_generation] def getConstructorPatternsFromAndOps[M, T](using
    quotes: Quotes,
    tm: Type[M],
    tt: Type[T]
)(
    unapplyTree: quotes.reflect.Unapply
): List[quotes.reflect.TypedOrTest] =
  import quotes.reflect.*

  val (left, right) =
    unapplyTree match
      case Unapply(_fun, _implicits, left :: right :: List()) => (left, right)
      case err =>
        report.errorAndAbort(
          s"Expected `Pattern1 &:& Pattern2` but found: ${err.show(using Printer.TreeStructure)}"
        )

  val rightTot = right match
    case tot: TypedOrTest => tot
    case other =>
      report.errorAndAbort(
        s"Right side of &:& must be a typed pattern like `MsgType(...)`, found: ${other.show(using Printer.TreeStructure)}"
      )

  left match
    case leftTot: TypedOrTest => List(leftTot, rightTot)
    case leftUnapply: Unapply =>
      getConstructorPatternsFromAndOps[M, T](leftUnapply) :+ rightTot
    case err =>
      report.errorAndAbort(
        s"Left side of &:& must be a typed pattern or another &:& expression, found: ${err.show(using Printer.TreeStructure)}"
      )

/** Traverses the AST of the `receive` block to extract all `CaseDef` match clauses
  * and convert them into join pattern expressions.
  *
  * Expects the shape: `receive { (self: ActorRef[M]) => { case ... => ... } }`
  *
  * @param expr
  *   the quoted receive block expression.
  * @return
  *   a list of join pattern expressions, one per case clause.
  */
private[code_generation] def getJoinDefinition[M, T](
    expr: Expr[ActorRef[M] => PartialFunction[Any, T]]
)(using quotes: Quotes, tm: Type[M], tt: Type[T]): List[Expr[JoinPattern[M, T]]] =
  import quotes.reflect.*
  expr.asTerm match
    case Inlined(_, _, Inlined(_, _, Block(_, Block(stmts, _)))) =>
      stmts match
        case (defn @ DefDef(_, List(TermParamClause(params)), _, Some(Block(_, Block(body, _))))) :: _ =>
          body match
            case DefDef(_, _, _, Some(Match(_, cases))) :: _ =>
              val selfSym = params match
                case p :: _ => p.symbol
                case Nil =>
                  report.errorAndAbort(
                    "Expected receive { (self: ActorRef[M]) => ... } but the function has no parameters"
                  )
              val jps = cases.flatMap(`case` => generateJoinPattern[M, T](`case`, selfSym))
              jps
            case _ =>
              errorTreeWithHint(
                "Unexpected structure inside receive block",
                "Expected pattern match cases: { case Msg(x) => ... }",
                defn
              )
              List()
        case default :: _ =>
          errorTreeWithHint(
            "Unsupported code inside receive block",
            "Expected: receive { (self: ActorRef[M]) => { case ... } }(matcher)",
            default
          )
          List()
        case Nil =>
          report.errorAndAbort("Empty receive block: expected at least one pattern case")
    case default =>
      errorTreeWithHint(
        "Unsupported expression passed to receive macro",
        "Expected: receive { (self: ActorRef[M]) => { case ... } }(matcher)",
        default
      )
      List()
