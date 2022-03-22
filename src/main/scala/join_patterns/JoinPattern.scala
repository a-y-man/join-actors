package join_patterns

import java.util.concurrent.LinkedTransferQueue
import scala.quoted.*
import join_patterns.Message
import scala.compiletime.error

/** Checks that if there is a guard, it is a function application.
 *
 *  @param guard the guard to check.
 *  @return an `Option` containing an error message, or `None`
 */
def checkGuard(using Quotes)(guard: Option[quotes.reflect.Term]): Option[String] = {
	import quotes.reflect.*
	import scala.compiletime.codeOf

	guard match
		case Some(Apply(Select(_, "apply"), _)) | None => None
		case Some(t) => Some("Guard was not a function application, got : " + t)
}

/** Inspects an `Expr` to ensure it is valid, using reflection.
 *
 *  @param expr the expression to inspect.
 *  @see https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html#pattern-matching-on-quoted-expressions
 */
def inspect[T: Type](expr: Expr[T])(using Quotes) = {
	import quotes.reflect.*

	expr.asTerm match
		case Inlined(_, _, Block(_, Block(stmts, _))) => {
			stmts(0) match
				case DefDef(_, _, _, Some(Block(_, Match(_, cases)))) => {
					for (_case <- cases) {
						_case match
							case CaseDef(pattern, guard, _) =>
								checkGuard(guard) match
									case Some(s) => report.error(s)
									case _ => ()
					}
				}
		}
}

def inspect2[T: Type](x: Expr[T])(using Quotes) = {
	import quotes.reflect.*
	import scala.language.postfixOps

	x match
		case '{ $y: (Message => T) } =>
			println("first")
			//println(prettyPrint(y.asTerm.tpe))
			val a = y.asTerm.asInstanceOf[Block]
			print(a)
		//case '{ ${expr @ Expr(value)}: Inlined } => ()
		case '{ $block0: t } =>
			println(Type.show[t])
			println()
			println(block0)
			println("second")
}

def generate[T: Type](x: Expr[T])(using Quotes): Expr[LinkedTransferQueue[Message] => T] = '{
	(queue: LinkedTransferQueue[Message]) => ${x}
}

/** Implementation of the `receive` macro, in two steps: `inspect` and `generate`.
 *
 *  @param expr the block to use as source of the pattern-matching code.
 *  @return a comptime function performing pattern-matching on a message queue at runtime.
 */
def _receive[T: Type](expr: Expr[T])(using Quotes): Expr[LinkedTransferQueue[Message] => T] = {
	inspect(expr)
	val out = generate(expr)
	_println(out)
	out
}

/** Entry point of the `receive` macro.
 *
 *  @param expr the block to use as source of the pattern-matching code.
 *  @return a comptime function performing pattern-matching on a message queue at runtime.
 */
inline def receive[T](inline expr : T): (LinkedTransferQueue[Message] => T) = ${_receive('{expr})}

/*
 primitive / composite event

val p = (C c -> D d)
val p1[Ty] = Ty c

receive(e) {
	d: D => println(""),
	D(d0, d1, d2) => println(""),
	(A a, B b, C c) => println(""), // local copies, used data is marked for `this` but not consumed (so others can use it)
	(A a & B b & C c) => println(""), // all present at the same time in LinkedTransferQueue[Message]
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