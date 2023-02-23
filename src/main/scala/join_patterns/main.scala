package join_patterns

import java.util.concurrent.LinkedTransferQueue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer

sealed abstract class Msg
case class A()                                      extends Msg
case class B()                                      extends Msg
case class C()                                      extends Msg
case class D(a: Int)                                extends Msg
case class E(a: Int)                                extends Msg
case class F(a: Int)                                extends Msg
case class G(b: Int, a: String, c: Int, d: Boolean) extends Msg

def test01(algorithm: AlgorithmType): Unit =
  println(s"Using ${algorithm}\n\n")

  val q = LinkedTransferQueue[Msg]

  var rcv = receive { (y: Msg) =>
    y match
      // case A() => println("Singleton")
      // case (D(x: Int), E(y: Int)) => println("Debug")
      case (D(x: Int), E(y: Int), D(z: Int)) => println(s"Case 00: x = ${x}, y = ${y}, z = ${z}")
    // case (D(x: Int), F(z: Int), E(y: Int)) => println(s"Case 01: x = ${x}, y = ${y}, z = ${z}")
    // case (E(y: Int), D(x: Int), F(z: Int)) => println(s"Case 02: x = ${x}, y = ${y}, z = ${z}")
    // case (E(y: Int), F(z: Int), D(x: Int)) => println(s"Case 03: x = ${x}, y = ${y}, z = ${z}")
    // case (F(z: Int), D(x: Int), E(y: Int)) => println(s"Case 04: x = ${x}, y = ${y}, z = ${z}")
    // case (F(z: Int), E(y: Int), D(x: Int)) => println(s"Case 05: x = ${x}, y = ${y}, z = ${z}")
  }

  val matcher = rcv(algorithm)
  // q.add(A())
  // q.add(B())
  // q.add(C())
  q.add(D(3))
  q.add(E(1))
  q.add(D(2))

  val initalQ = q.toArray.toList.zipWithIndex
  println(s"Q =  ${initalQ}")
  println(f"f returned: ${matcher(q)}")
  println("\n======================================================\n\n")

def test02(algorithm: AlgorithmType): Unit =
  println(s"Using ${algorithm}\n\n")
  val q = LinkedTransferQueue[Msg]

  // This will return a lambda that takes algorithm type and returns matcher
  var rcv =
    receive { (y: Msg) =>
      y match
        case (A(), A(), A(), A(), A(), A(), A(), A(), A()) => println("Match!")
    }

  val matcher = rcv(algorithm)

  q.add(A())
  q.add(A())
  q.add(A())
  q.add(A())
  q.add(A())
  q.add(A())
  q.add(A())
  q.add(A())
  q.add(A())

  val initalQ = q.toArray.toList.zipWithIndex

  println(s"Q =  ${initalQ}")
  println(f"receive = ${matcher(q)}")
  println("\n======================================================\n\n")

@main
def main(): Unit =
  // test01(AlgorithmType.BasicAlgorithm)
  test01(AlgorithmType.TreeBasedAlgorithm)
  // test02(AlgorithmType.BasicAlgorithm)
  // test02(AlgorithmType.TreeBasedAlgorithm)
// def test02(): Unit =
//   val i: Int                 = 0;
//   val m                      = 0
//   val isZero: Int => Boolean = (n: Int) => n == 0
//   val q                      = LinkedTransferQueue[Msg]

//   var f = receive { (y: Msg) =>
//     y match
//       case E(n: Int) if n == 2                  => { { val z = "hi"; println(z) }; n + 1 }
//       case (A(), B(), A(), E(n: Int)) if n == 2 => 500 * n
//       case (B(), A(), B(), E(n: Int)) if n == 2 => 600 * n
//   }

//   // A E E B A B
//   q.add(A())
//   q.add(E(2))
//   q.add(E(2))
//   q.add(B())
//   q.add(A())
//   q.add(B())

//   val initalQ = q.toArray.toList.zipWithIndex
//   println(s"Q =  ${initalQ}")
//   println(s"f returned: ${f(q)}")

//   println("\n======================================================\n\n")

// def test04()=
//   val result                 = Random.nextInt
//     val isZero: Int => Boolean = (n: Int) => n == 0
//     val rcv = receive { (y: Msg) =>
//       y match
//         case (A(), D(), E(b: Int)) if isZero(0) => result + 1
//         case (A(), D(), E())              => result
//     }
//     val q = LinkedTransferQueue[Msg]

//     q.add(A())
//     q.add(D())
//     q.add(E())

//   val initalQ = q.toArray.toList.zipWithIndex
//   println(s"Q =  ${initalQ}")
//   println(s"f returned: ${f(q)}")

//   println("\n======================================================\n\n")

// def demo(): Unit =
//   val queue = LinkedTransferQueue[Msg]()

//   val f = receive { (msg: Msg) =>
//     msg match
//       case (A(), B(), C())     => println(s"I've received 3 messages: A, B and C :)")
//       case D(n: Int) if n > 0  => println(s"I've received one message with the payload ${n} :)")
//       case E(n: Int) if n != n => println(s"I cannot happen :(")
//       case (F(a: Int), E(b: Int)) if (a + b == 42) =>
//         println(s"I've received 2 messages with the same payload :)")
//   }
//   queue.add(F(21))
//   queue.add(E(21))

//   queue.add(A())
//   queue.add(B())
//   queue.add(C())

//   // queue.add(D(42))

//   // queue.add(E(2))

//   println(s"f returned: ${f(queue)}")
