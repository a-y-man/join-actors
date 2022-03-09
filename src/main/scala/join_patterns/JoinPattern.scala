package join_patterns

import java.util.concurrent.LinkedTransferQueue
import scala.quoted.*
import join_patterns.Message

case class Login(id: String) extends Message
case class Logout(id: String) extends Message
case class OAuth(id: String) extends Message

def _println[T](x: Expr[T])(using Quotes) = {
	import quotes.reflect.*

	val tree: Tree = x.asTerm
	println(tree.show(using Printer.TreeStructure))
	x
}

/**
 * Check thqt casedef guard is an Apply()
*/
def _match[T <: Message](x: Expr[Seq[T => Unit]])(using Quotes) =
	import quotes.reflect.*

	x match
		case CaseDef(pattern, guard, handler) => x
		case _ => println(f"x")

/*
def handleBody[R](y: Expr[R])(using Quotes): Unit =
	y match
		case '{ x match $mc } => handleMatchClauses(mc) // mc: Expr[R]
*/

// top-level
inline def receive[R](queue: LinkedTransferQueue[Message])(inline expr : R): Unit = ${
	_println('{expr}) // see what is there
	//_match('{expr}) // check well-formed
	// generate code
}

/*
 primitive / composite event

val p = (C c -> D d)
val p1[Ty] = Ty c

receive(e) {
	D d => println(""),
	D(d0, d1, d2) => println(""),
	(A a, B b, C c) => println(""), // local copies, used data is marked for `this` but not consumed (so others can use it)
	(A a & B b & C c) => println(""), // all present at the same time in queue
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