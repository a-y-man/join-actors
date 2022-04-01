package join_patterns

import org.scalatest.funsuite._
import java.util.concurrent.LinkedTransferQueue

case class Get() extends Message
case class Set(/*element: Int*/) extends Message

class Cell[T](queue: LinkedTransferQueue[Message]) {
	var element: Option[T] = None

	val isDefined = (e: Option[T]) => e.isDefined
	val isEmpty = (e: Option[T]) => e.isEmpty

	receive {
		(x: Message) => x match
			case Get() if isDefined(element) => println(element)
			case Get() if isEmpty(element) => println("empty")
			case Set() => println("Set")
			//case _ => println("unknown message")
	}
}

class CellTest() extends AnyFunSuite {
	test("CellTest") {
		val queue = LinkedTransferQueue[Message]()
		var cell = Cell[Int](queue)
	}
}