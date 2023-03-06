package join_patterns

import java.util.concurrent.LinkedTransferQueue

sealed abstract class Msg
case class A()                                      extends Msg
case class B()                                      extends Msg
case class C()                                      extends Msg
case class D(a: Int)                                extends Msg
case class E(a: Int)                                extends Msg
case class F(a: Int)                                extends Msg
case class G(b: Int, a: String, c: Int, d: Boolean) extends Msg

def demo(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val queue = LinkedTransferQueue[Msg]()

  val rcv = receive { (msg: Msg) =>
    msg match
      case (A(), B(), C())     => println(s"I've received 3 messages: A, B and C :)")
      case D(n: Int) if n > 0  => println(s"I've received one message with the payload ${n} :)")
      case E(n: Int) if n != n => println(s"I cannot happen :(")
      case (F(a: Int), E(b: Int)) if (a + b == 42) =>
        println(s"I've received 2 messages with the same payload :)")
  }

  val matcher = rcv(algorithm)

  queue.add(F(21))
  queue.add(E(21))

  queue.add(A())
  queue.add(B())
  queue.add(C())

  // queue.add(D(42))

  // queue.add(E(2))

  println(s"f returned: ${matcher(queue)}")


@main
def main(): Unit =
  demo(MatchingAlgorithm.BasicAlgorithm)
  demo(MatchingAlgorithm.TreeBasedAlgorithm)


