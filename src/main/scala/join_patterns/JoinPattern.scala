package join_patterns

import java.util.concurrent.LinkedTransferQueue
import scala.quoted.*
import join_patterns.Message

def _println[T](x: Expr[T])(using Quotes) = {
	import quotes.reflect.*

	val tree: Tree = x.asTerm
	//println(tree.show(using Printer.TreeStructure))
	println(prettyPrint(tree))
}

/**
 * Check that casedef guard is an Apply()
 * https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html#pattern-matching-on-quoted-expressions
*/
def _match[T](x: Expr[T])(using Quotes) = {
	import quotes.reflect.*

	x.asTerm match
		case Inlined(_, _, Block(_, Block(n, _))) => {
			n(0) match
				case DefDef(_, _, _, Some(b)) => b match
					case Block(_, Match(_, cases)) => {
						for (_case <- cases)
							println(prettyPrint(_case))
					}
		}

	// generate code
	x
}

// top-level
inline def receive[R](queue: LinkedTransferQueue[Message])(inline expr : R): Unit = ${
	_match('{expr}) // check well-formed
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