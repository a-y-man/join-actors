package join_patterns.code_generation

import join_actors.actor.ActorRef
import join_patterns.types.*

import scala.quoted.{Expr, Quotes, Type}

/** Creates the right-hand side closure for a join pattern.
  *
  * Generates a lambda `(LookupEnv, ActorRef[M]) => T` that:
  * 1. Substitutes the `self` reference with the actual `ActorRef` parameter
  * 2. Replaces bound variable references with `LookupEnv` lookups, cast to their original types
  *
  * @param rhs
  *   the right-hand side term from the case clause.
  * @param fieldBindings
  *   the field names and types available in this pattern.
  * @param selfRefName
  *   the name of the self ActorRef parameter to substitute.
  * @return
  *   a `Block` containing the RHS lambda.
  */
private[code_generation] def generateRhs[M, T](using
    quotes: Quotes,
    tt: Type[T],
    tm: Type[M]
)(
    rhs: quotes.reflect.Term,
    fieldBindings: List[(String, quotes.reflect.TypeRepr)],
    selfRefName: String
): quotes.reflect.Block =
  import quotes.reflect.*

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("_", s"$selfRefName"))(
      _ =>
        List(
          TypeRepr.of[LookupEnv],
          TypeRepr.of[ActorRef[M]]
        ),
      _ => TypeRepr.of[T]
    ),
    rhsFn = (sym: Symbol, params: List[Tree]) =>
      val (lookupEnv, actorRefObj) = params match
        case (id: Ident) :: ref :: _ => (id, ref.asExprOf[ActorRef[M]].asTerm)
        case _ =>
          report.errorAndAbort(
            "Internal macro error: generateRhs expected (LookupEnv, ActorRef) parameters"
          )
      val rhsWithSelf = substitute(rhs, selfRefName, actorRefObj)(sym)
      val transform = new TreeMap:
        override def transformTerm(term: Term)(owner: Symbol): Term = term match
          case Ident(n) if fieldBindings.exists(_._1 == n) =>
            val inner = '{ (${ lookupEnv.asExprOf[LookupEnv] })(${ Expr(n) }) }
            fieldBindings.find(_._1 == n).map(_._2.asType) match
              case Some(tpe) =>
                tpe match
                  case '[innerType] => ('{ ${ inner }.asInstanceOf[innerType] }).asTerm
              case None =>
                report.errorAndAbort(
                  s"Internal macro error: variable '$n' not found in pattern bindings"
                )
          case x => super.transformTerm(x)(owner)

      transform.transformTerm(rhsWithSelf.changeOwner(sym))(sym)
  )
