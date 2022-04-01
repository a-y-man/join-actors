package join_patterns

import org.scalatest.funsuite._
import java.util.concurrent.LinkedTransferQueue

case class Login(/*id: String*/) extends Message
case class Logout(/*id: String*/) extends Message
case class OAuth(/*id: String*/) extends Message

class LoginTest() extends AnyFunSuite {
	test("LoginTest") {
		val queue: LinkedTransferQueue[Message] = LinkedTransferQueue[Message]()
		val checkId = (id: String) => id.size > 0

		receive {
		(x: Message) => x match
			case Login() /*if checkId(l.id)*/ => println(""/*l.id*/)
			case Logout() => println("here")
			case OAuth() => {
				val y = 1
				println(y)
			}
		}
	}
}