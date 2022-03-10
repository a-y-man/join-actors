package join_patterns

import org.scalatest.funsuite._
import java.util.concurrent.LinkedTransferQueue

case class Get() extends Message
case class Set[T](element: T) extends Message

class Cell[T](queue: LinkedTransferQueue[Message]) {
	var element: Option[T] = None

	receive(queue) {
		(x: Message) => x match
		case Get() if element != None => println(element)
		case Get() if element == None => println("empty")
		case Set(new_element) => println("Set")
		case _ => println("unknown message")
	}
}

class CellTest() extends AnyFunSuite {
	test("CellTest") {
		val queue: LinkedTransferQueue[Message] = LinkedTransferQueue[Message]()
		var cell: Cell[Int] = Cell[Int](queue)
	}
}