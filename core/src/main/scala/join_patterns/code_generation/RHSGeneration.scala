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
  * Both substitutions are by symbol, not by name: only references to the pattern variable's own
  * `Bind` (or to the `self` parameter of the `receive` function) are replaced. A local `val`,
  * `def`, lambda parameter or nested pattern variable that happens to have the same name is left
  * alone.
  *
  * @param rhs
  *   the right-hand side term from the case clause.
  * @param bindings
  *   the pattern variables of the case: name, symbol of the `Bind`, and type of the field.
  * @param selfSym
  *   the symbol of the self ActorRef parameter to substitute.
  * @return
  *   a `Block` containing the RHS lambda.
  */
private[code_generation] def generateRhs[M, T](using
    quotes: Quotes,
    tt: Type[T],
    tm: Type[M]
)(
    rhs: quotes.reflect.Term,
    bindings: List[(String, quotes.reflect.Symbol, quotes.reflect.TypeRepr)],
    selfSym: quotes.reflect.Symbol
): quotes.reflect.Block =
  import quotes.reflect.*

  val bySymbol: Map[Symbol, (String, TypeRepr)] =
    bindings.map((name, sym, tpe) => sym -> (name, tpe)).toMap

  Lambda(
    owner = Symbol.spliceOwner,
    tpe = MethodType(List("_", selfSym.name))(
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
      val lookupEnvExpr = lookupEnv.asExprOf[LookupEnv]
      val transform = new TreeMap:
        override def transformTerm(term: Term)(owner: Symbol): Term = term match
          case id: Ident if id.symbol == selfSym =>
            actorRefObj.changeOwner(owner)
          case id: Ident if bySymbol.contains(id.symbol) =>
            val (name, tpe) = bySymbol(id.symbol)
            lookupBinding(lookupEnvExpr, name, tpe)
          case x => super.transformTerm(x)(owner)

      transform.transformTerm(rhs.changeOwner(sym))(sym)
  )
