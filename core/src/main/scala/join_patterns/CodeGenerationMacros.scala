package join_patterns.code_generation

import join_actors.actor.{ActorRef, Result}
import join_patterns.matching.*
import join_patterns.types.{*, given}

import scala.quoted.{Expr, Quotes, Type}

/** Marker infix operator for composing multi-message join patterns.
  *
  * Enables syntax: `case A(x) &:& B(y) &:& C(z) => ...`
  * The `unapply` always succeeds — the macro intercepts and processes the composed patterns.
  */
object `&:&`:
  infix def unapply(arg: Any): Option[(Unit, Unit)] = Some((), ())

/** Generates the code returned by the `receive` macro.
  * Orchestrates AST extraction and join pattern code generation,
  * then wraps the result in a `MatcherFactory` invocation.
  */
private def receiveCodeGen[M, T](
    jpsExpr: Expr[ActorRef[M] => PartialFunction[Any, Result[T]]]
)(matcherConstructor: Expr[MatcherFactory])(using
    tm: Type[M],
    tt: Type[T],
    quotes: Quotes
): Expr[Matcher[M, Result[T]]] =
  '{
    val jps: JoinDefinition[M, Result[T]] =
      ${
        Expr.ofList(
          getJoinDefinition(
            jpsExpr.asInstanceOf[Expr[ActorRef[M] => PartialFunction[Any, Result[T]]]]
          )
        )
      }

    val matcher = (${ matcherConstructor }.apply[M, Result[T]])(jps)
    matcher
  }

/** Entry point of the `receive` macro.
  *
  * Transforms a block of pattern-matching code using join pattern syntax
  * into a `Matcher` instance that performs runtime pattern matching on a message queue.
  *
  * @param f
  *   the block containing join pattern cases.
  * @param createMatcher
  *   the matcher factory to instantiate the runtime matcher.
  * @return
  *   a `Matcher` that matches messages against the defined join patterns.
  */
inline def receive[M, T](
    inline f: (ActorRef[M] => PartialFunction[Any, Result[T]])
)(inline createMatcher: MatcherFactory): Matcher[M, Result[T]] =
  ${ receiveCodeGen('f)('createMatcher) }
