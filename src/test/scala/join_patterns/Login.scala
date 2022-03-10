package join_patterns

import org.scalatest.funsuite._
import java.util.concurrent.LinkedTransferQueue

case class Login(id: String) extends Message
case class Logout(id: String) extends Message
case class OAuth(id: String) extends Message

class LoginTest() extends AnyFunSuite {
	test("LoginTest") {
		val queue: LinkedTransferQueue[Message] = LinkedTransferQueue[Message]()

		receive (queue) {
		(x: Message) => x match
			case Login(name) if name != "" => println(name)
			case (l: Logout) => println("here")
			case (a: OAuth) => {
				val y = 1
				println(y)
			}
		}
	}
}