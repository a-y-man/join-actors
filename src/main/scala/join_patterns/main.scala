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

// object PatternOrdering extends Ordering[JoinPattern[Msg, Int]] {
//   def compare(a: JoinPattern[Msg, Int], b: JoinPattern[Msg, Int]) = a.size.compare(b.size)
// }

// def test01(): Unit =
//   val q = LinkedTransferQueue[Msg]

//   var f = receive { (y: Msg) =>
//     y match
//       case (D(x: Int), E(y: Int), F(z: Int)) => 1 + x * y * z      // 0
//       case (D(x: Int), F(y: Int), E(z: Int)) => 2 + x * y * z      // 1
//       case (F(x: Int), D(y: Int), E(z: Int)) => 3 + x * y * z      // 2
//       case (E(x: Int), D(y: Int), F(z: Int)) => 4 + x * y * z      // 3
//       case (F(x: Int), E(y: Int), D(z: Int)) => 5 + x * y * z      // 4
//       case (E(x: Int), F(y: Int), D(z: Int)) => 6 + z + 1          // 5
//       case (E(x: Int), F(y: Int), D(z: Int)) => 6 + x * y * z      // 6
//       case (E(x: Int), F(y: Int), D(z: Int)) => 6 + z + 1234567890 // 7
//       case (A(), B(), A())                   => 42
//   }

//   q.add(A())
//   // q.add(E(1))
//   // q.add(F(2))
//   q.add(B())
//   q.add(A())
//   // q.add(D(3))

//   val initalQ = q.toArray.toList.zipWithIndex
//   println(s"Q =  ${initalQ}")
//   println(f"f returned: ${f(q)}")
//   // println(f"f returned after: ${f(q)}")

  // println("\n======================================================\n\n")

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

def testPartial(algorithm: AlgorithmType): Unit =
  val q = LinkedTransferQueue[Msg]

  var matcher = receive { (y: Msg) =>
    y match
      case (E(x: Int), D(z: Int), E(y: Int)) => println("Case 0"); x
      case (D(z: Int), E(x: Int)) if x == 42 => println("Case 1"); x
      case (E(x: Int), A(), D(y: Int)) if x == 84 => println("Case 2"); x
      case (E(x: Int), A(), G(b: Int, a: String, c: Int, d: Boolean)) if x == c => println("Case 3"); x
      // case G(_: Int, a: String, c: Int, d: Boolean) => println("Case 4"); 42
  }

  q.add(C())
  q.add(C())
  q.add(G(42, "Hi", 1, true))
  q.add(B())
  q.add(A())
  q.add(E(1))
  q.add(D(42))
  q.add(E(2))
  q.add(E(3))
  q.add(E(84))
  val initalQ = q.toArray.toList.zipWithIndex

  // val f : TreeMatcher[Msg, Int] = matcher(AlgorithmType.TreeBasedAlgorithm)
  println(s"Q =  ${initalQ}")
  println(f"receive = ${matcher(q)}")
  println("\n======================================================\n\n")

@main
def main(): Unit =
  testPartial(AlgorithmType.TreeBasedAlgorithm)
