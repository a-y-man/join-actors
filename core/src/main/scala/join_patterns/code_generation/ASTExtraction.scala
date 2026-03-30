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
      errorTreeWithHint(
        s"Unsupported payload binding",
        "Expected `name: Type`, `_: Type`, or `_` wildcard",
        t
      )
      ("", TypeRepr.of[Nothing])

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
    case TypedOrTest(Unapply(Select(s, "unapply"), _, binds), tt: TypeTree) =>
      tt.tpe.dealias.simplified match
        case tp: TypeRef =>
          tp -> binds.map(extractPayloads(_))
    case default =>
      errorTreeWithHint(
        "Unsupported message constructor type",
        "Expected a case class pattern like `MsgType(field1, field2)`",
        default
      )
      TypeRepr.of[Nothing] -> List()
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
              val selfRefName = params match
                case p :: _ => p.name
                case Nil =>
                  report.errorAndAbort(
                    "Expected receive { (self: ActorRef[M]) => ... } but the function has no parameters"
                  )
              val jps = cases.flatMap(`case` => generateJoinPattern[M, T](`case`, selfRefName))
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
