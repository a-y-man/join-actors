package join_patterns

import java.util.concurrent.LinkedTransferQueue

@main
def main(): Unit =
	println("Hello, world")

	val queue: LinkedTransferQueue[Message] = LinkedTransferQueue[Message]()
	val i: Int = 0;

	val iZero: Int => Boolean = (i: Int) => i == 0
	val iOne: Int => Boolean = (i: Int) => i == 0

	receive (queue) {
		(x: Message) => x match
			case c: Any if i == 0 => 8
			case c: Any if iOne(i) => 9
	}

