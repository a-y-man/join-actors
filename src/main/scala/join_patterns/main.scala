package join_patterns

import java.util.concurrent.LinkedTransferQueue

/*
def test(using Quotes): Unit =
	import quotes.reflect.*

	val x = Literal(IntConstant(10))
	val _println = Apply(Ref(Symbol.requiredMethod("println")),
		List(Typed(Inlined(None, Nil, Repeated(List(Literal(IntConstant(10)),
			Literal(StringConstant("str")),
			Literal(DoubleConstant(5.2))),
			TypeTree.of[Any])),
			Applied(TypeIdent(defn.RepeatedParamClass), List(TypeTree.of[Any])))))
	val cases: List[CaseDef] = List(CaseDef(Literal(IntConstant(5)), None, _println))
	val tree: Tree = Match(x, cases)

	println(tree.show(using Printer.TreeStructure))
*/

@main
def main(): Unit =
	println("Hello, world")

	val queue: LinkedTransferQueue[Message] = LinkedTransferQueue[Message]()
	val i: Int = 0;

	receive (queue) {
		(x: Message) => x match
			case c: Any if i == 0 => 8
			case c: Any if i == 1 => 9
	}

