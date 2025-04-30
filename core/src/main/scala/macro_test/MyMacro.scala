package macro_test

import join_actors.actor.*

import scala.quoted.*

object MyMacro:
  inline def myMacro[M, T](inline f: (ActorRef[M] => PartialFunction[Any, Result[T]])): Any = ${ myMacroImpl('f) }

  def myMacroImpl[M, T](a: Expr[ActorRef[M] => PartialFunction[Any, Result[T]]])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    org.felher.s3te.S3te.renderTreeHTML("asts/tree.html", a.asTerm)

    '{ null }