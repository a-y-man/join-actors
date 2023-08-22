package join_patterns
import java.util.concurrent.LinkedTransferQueue
import scala.util.Random

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

  println(s"Matcher returned: ${matcher(queue)}")

def test01(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val q = LinkedTransferQueue[Msg]

  var rcv = receive { (y: Msg) =>
    y match
      case (D(x: Int), E(y: Int), F(z: Int)) => println(s"Case 00: x = ${x}, y = ${y}, z = ${z}")
      case (D(x: Int), F(z: Int), E(y: Int)) => println(s"Case 01: x = ${x}, y = ${y}, z = ${z}")
      case (E(y: Int), D(x: Int), F(z: Int)) => println(s"Case 02: x = ${x}, y = ${y}, z = ${z}")
      case (E(y: Int), F(z: Int), D(x: Int)) => println(s"Case 03: x = ${x}, y = ${y}, z = ${z}")
      case (F(z: Int), D(x: Int), E(y: Int)) => println(s"Case 04: x = ${x}, y = ${y}, z = ${z}")
      case (F(z: Int), E(y: Int), D(x: Int)) => println(s"Case 05: x = ${x}, y = ${y}, z = ${z}")
    // case _                                 => println("No match")
  }

  val matcher = rcv(algorithm)
  q.add(D(3))
  q.add(F(2))
  q.add(E(1))
  q.add(A())
  q.add(B())
  q.add(C())

  val initalQ = q.toArray.toList.zipWithIndex
  println(s"Q =  ${initalQ}")
  println(f"Matcher returned: ${matcher(q)}")
  println(s"Q =  ${q.toArray.toList.zipWithIndex}")
  println("\n======================================================\n\n")

def test02(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val q = LinkedTransferQueue[Msg]

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

def test03(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0
  val q                      = LinkedTransferQueue[Msg]

  var rcv = receive { (y: Msg) =>
    y match
      case (E(m: Int), E(n: Int)) if n == 2 && m == 42 => { { val z = "hi"; println(z) }; n + 1 }
      case (A(), B(), A(), E(n: Int)) if n == 2        => 500 * n
      case (B(), A(), B(), E(n: Int)) if n == 2        => 600 * n
  }

  val matcher = rcv(algorithm)

  // A E E B A B
  q.add(E(43))     // 3
  q.add(E(341231)) // 0
  q.add(E(231))    // 5
  q.add(A())       // 4
  q.add(E(2))      // 1
  q.add(E(42))     // 2

  val initalQ = q.toArray.toList.zipWithIndex
  println(s"Q =  ${initalQ}")
  println(s"Matcher returned: ${matcher(q)}")

  println("\n======================================================\n\n")

def test04(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0
  val q                      = LinkedTransferQueue[Msg]

  var rcv = receive { (y: Msg) =>
    y match
      case (E(m: Int), F(n: Int), E(o: Int)) => {
        { val z = "E(m: Int), F(n: Int), E(o: Int)"; println(z) }
      }
  }

  val matcher = rcv(algorithm)

  // q.add(A())
  // q.add(B())
  // q.add(A())
  q.add(E(4))
  q.add(F(2))
  q.add(E(1))

  val initalQ = q.toArray.toList.zipWithIndex
  println(s"Q =  ${initalQ}")
  println(s"Matcher returned: ${matcher(q)}")

  println("\n======================================================\n\n")

def test05(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val q = LinkedTransferQueue[Msg]

  var rcv =
    receive { (y: Msg) =>
      y match
        case (
              E(a: Int),
              E(b: Int),
              E(c: Int),
              E(d: Int),
              E(e: Int),
              E(f: Int),
              E(g: Int),
              E(h: Int),
              E(i: Int),
              E(j: Int)
            )
            if a == 10 && b == 9 && c == 8 && d == 7 && e == 6 && f == 5 && g == 4 && h == 3 && i == 2 && j == 1 =>
          println("Match!")
    }

  val matcher = rcv(algorithm)

  q.add(E(10))
  q.add(E(9))
  q.add(E(8))
  q.add(E(7))
  q.add(E(6))
  q.add(E(5))
  q.add(E(4))
  q.add(E(3))
  q.add(E(2))
  q.add(E(1))

  // q.add(E(1))
  // q.add(E(2))
  // q.add(E(3))
  // q.add(E(4))
  // q.add(E(5))
  // q.add(E(6))
  // q.add(E(7))
  // q.add(E(8))
  // q.add(E(9))
  // q.add(E(10))

  val initalQ = q.toArray.toList.zipWithIndex

  println(s"Q =  ${initalQ}")
  println(f"receive = ${matcher(q)}")
  println("\n======================================================\n\n")

def test06(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val result = 42
  val rcv = receive { (y: Msg) =>
    y match
      case (F(i0: Int), E(i1: Int)) if i0 == i1 =>
        result
      case (F(i0: Int), G(i1: Int, s1: String, i2: Int, b: Boolean)) if i0 == i1 && s1 == s1 && b =>
        result + 1
  }
  val matcher = rcv(algorithm)
  val q       = LinkedTransferQueue[Msg]

  q.add(B())
  q.add(A())
  q.add(F(4))
  q.add(G(1, "G", 1, false))
  q.add(B())
  q.add(E(1))
  q.add(E(2))
  q.add(E(3))
  q.add(E(4))
  q.add(E(5))
  q.add(E(42))
  q.add(G(42, "G", 1, true))
  q.add(F(42))

  // println(s"Q =  ${q.toArray.toList.zipWithIndex}")
  println(s"Q =  ${q.toArray.toList.zipWithIndex}")
  println(f"receive = ${matcher(q)}")
  println("\n======================================================\n\n")

def test07(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val result = 42
  val rcv = receive { (y: Msg) =>
    y match
      case (F(i0: Int), E(i1: Int), F(i2: Int)) if i0 == i1 && i1 == i2 =>
        result
      case F(a: Int) => result * a
  }

  val matcher = rcv(algorithm)
  val q       = LinkedTransferQueue[Msg]

  q.add(F(4))
  q.add(E(4))
  q.add(F(4))

  println(result)
  println(s"Q =  ${q.toArray.toList.zipWithIndex}")
  println(f"receive = ${matcher(q)}")
  println("\n======================================================\n\n")

def test08(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val rcv = receive { (y: Msg) =>
    y match
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 3 && b == 2 && c == 1    => a * b * c
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 4 && b == 5 && c == 6    => a * b * c
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 7 && b == 8 && c == 9    => a * b * c
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 16 && b == 32 && c == 64 => a * b * c
  }

  val matcher = rcv(algorithm)
  val q       = LinkedTransferQueue[Msg]

  q.add(E(0))  // 0
  q.add(E(0))  // 1
  q.add(E(0))  // 2
  q.add(E(0))  // 3
  q.add(E(0))  // 4
  q.add(E(0))  // 5
  q.add(E(64)) // 6
  q.add(E(32)) // 7
  q.add(E(16)) // 8

  println(s"Q =  ${q.toArray.toList.zipWithIndex}")
  println(f"receive = ${matcher(q)}")
  println("\n======================================================\n\n")

/*
[0, 1, 2] --> [ [0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0] ]
[0, 1, 3] --> [ [0, 1, 3], [0, 3, 1], [1, 0, 3], [1, 3, 0], [3, 0, 1], [3, 1, 0] ]
[0, 1, 4] --> [ [0, 1, 4], [0, 4, 1], [1, 0, 4], [1, 4, 0], [4, 0, 1], [4, 1, 0] ]
[0, 1, 5] --> [ [0, 1, 5], [0, 5, 1], [1, 0, 5], [1, 5, 0], [5, 0, 1], [5, 1, 0] ]
[0, 1, 6] --> [ [0, 1, 6], [0, 6, 1], [1, 0, 6], [1, 6, 0], [6, 0, 1], [6, 1, 0] ]
[0, 1, 7] --> [ [0, 1, 7], [0, 7, 1], [1, 0, 7], [1, 7, 0], [7, 0, 1], [7, 1, 0] ]
[0, 1, 8] --> [ [0, 1, 8], [0, 8, 1], [1, 0, 8], [1, 8, 0], [8, 0, 1], [8, 1, 0] ]
[0, 2, 3] --> [ [0, 2, 3], [0, 3, 2], [2, 0, 3], [2, 3, 0], [3, 0, 2], [3, 2, 0] ]
[0, 2, 4] --> [ [0, 2, 4], [0, 4, 2], [2, 0, 4], [2, 4, 0], [4, 0, 2], [4, 2, 0] ]
[0, 2, 5] --> [ [0, 2, 5], [0, 5, 2], [2, 0, 5], [2, 5, 0], [5, 0, 2], [5, 2, 0] ]
[0, 2, 6] --> [ [0, 2, 6], [0, 6, 2], [2, 0, 6], [2, 6, 0], [6, 0, 2], [6, 2, 0] ]
[0, 2, 7] --> [ [0, 2, 7], [0, 7, 2], [2, 0, 7], [2, 7, 0], [7, 0, 2], [7, 2, 0] ]
[0, 2, 8] --> [ [0, 2, 8], [0, 8, 2], [2, 0, 8], [2, 8, 0], [8, 0, 2], [8, 2, 0] ]
[0, 3, 4] --> [ [0, 3, 4], [0, 4, 3], [3, 0, 4], [3, 4, 0], [4, 0, 3], [4, 3, 0] ]
[0, 3, 5] --> [ [0, 3, 5], [0, 5, 3], [3, 0, 5], [3, 5, 0], [5, 0, 3], [5, 3, 0] ]
[0, 3, 6] --> [ [0, 3, 6], [0, 6, 3], [3, 0, 6], [3, 6, 0], [6, 0, 3], [6, 3, 0] ]
[0, 3, 7] --> [ [0, 3, 7], [0, 7, 3], [3, 0, 7], [3, 7, 0], [7, 0, 3], [7, 3, 0] ]
[0, 3, 8] --> [ [0, 3, 8], [0, 8, 3], [3, 0, 8], [3, 8, 0], [8, 0, 3], [8, 3, 0] ]
[0, 4, 5] --> [ [0, 4, 5], [0, 5, 4], [4, 0, 5], [4, 5, 0], [5, 0, 4], [5, 4, 0] ]
[0, 4, 6] --> [ [0, 4, 6], [0, 6, 4], [4, 0, 6], [4, 6, 0], [6, 0, 4], [6, 4, 0] ]
[0, 4, 7] --> [ [0, 4, 7], [0, 7, 4], [4, 0, 7], [4, 7, 0], [7, 0, 4], [7, 4, 0] ]
[0, 4, 8] --> [ [0, 4, 8], [0, 8, 4], [4, 0, 8], [4, 8, 0], [8, 0, 4], [8, 4, 0] ]
[0, 5, 6] --> [ [0, 5, 6], [0, 6, 5], [5, 0, 6], [5, 6, 0], [6, 0, 5], [6, 5, 0] ]
[0, 5, 7] --> [ [0, 5, 7], [0, 7, 5], [5, 0, 7], [5, 7, 0], [7, 0, 5], [7, 5, 0] ]
[0, 5, 8] --> [ [0, 5, 8], [0, 8, 5], [5, 0, 8], [5, 8, 0], [8, 0, 5], [8, 5, 0] ]
[0, 6, 7] --> [ [0, 6, 7], [0, 7, 6], [6, 0, 7], [6, 7, 0], [7, 0, 6], [7, 6, 0] ]
[0, 6, 8] --> [ [0, 6, 8], [0, 8, 6], [6, 0, 8], [6, 8, 0], [8, 0, 6], [8, 6, 0] ]
[0, 7, 8] --> [ [0, 7, 8], [0, 8, 7], [7, 0, 8], [7, 8, 0], [8, 0, 7], [8, 7, 0] ]
[1, 2, 3] --> [ [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1] ]
[1, 2, 4] --> [ [1, 2, 4], [1, 4, 2], [2, 1, 4], [2, 4, 1], [4, 1, 2], [4, 2, 1] ]
[1, 2, 5] --> [ [1, 2, 5], [1, 5, 2], [2, 1, 5], [2, 5, 1], [5, 1, 2], [5, 2, 1] ]
[1, 2, 6] --> [ [1, 2, 6], [1, 6, 2], [2, 1, 6], [2, 6, 1], [6, 1, 2], [6, 2, 1] ]
[1, 2, 7] --> [ [1, 2, 7], [1, 7, 2], [2, 1, 7], [2, 7, 1], [7, 1, 2], [7, 2, 1] ]
[1, 2, 8] --> [ [1, 2, 8], [1, 8, 2], [2, 1, 8], [2, 8, 1], [8, 1, 2], [8, 2, 1] ]
[1, 3, 4] --> [ [1, 3, 4], [1, 4, 3], [3, 1, 4], [3, 4, 1], [4, 1, 3], [4, 3, 1] ]
[1, 3, 5] --> [ [1, 3, 5], [1, 5, 3], [3, 1, 5], [3, 5, 1], [5, 1, 3], [5, 3, 1] ]
[1, 3, 6] --> [ [1, 3, 6], [1, 6, 3], [3, 1, 6], [3, 6, 1], [6, 1, 3], [6, 3, 1] ]
[1, 3, 7] --> [ [1, 3, 7], [1, 7, 3], [3, 1, 7], [3, 7, 1], [7, 1, 3], [7, 3, 1] ]
[1, 3, 8] --> [ [1, 3, 8], [1, 8, 3], [3, 1, 8], [3, 8, 1], [8, 1, 3], [8, 3, 1] ]
[1, 4, 5] --> [ [1, 4, 5], [1, 5, 4], [4, 1, 5], [4, 5, 1], [5, 1, 4], [5, 4, 1] ]
[1, 4, 6] --> [ [1, 4, 6], [1, 6, 4], [4, 1, 6], [4, 6, 1], [6, 1, 4], [6, 4, 1] ]
[1, 4, 7] --> [ [1, 4, 7], [1, 7, 4], [4, 1, 7], [4, 7, 1], [7, 1, 4], [7, 4, 1] ]
[1, 4, 8] --> [ [1, 4, 8], [1, 8, 4], [4, 1, 8], [4, 8, 1], [8, 1, 4], [8, 4, 1] ]
[1, 5, 6] --> [ [1, 5, 6], [1, 6, 5], [5, 1, 6], [5, 6, 1], [6, 1, 5], [6, 5, 1] ]
[1, 5, 7] --> [ [1, 5, 7], [1, 7, 5], [5, 1, 7], [5, 7, 1], [7, 1, 5], [7, 5, 1] ]
[1, 5, 8] --> [ [1, 5, 8], [1, 8, 5], [5, 1, 8], [5, 8, 1], [8, 1, 5], [8, 5, 1] ]
[1, 6, 7] --> [ [1, 6, 7], [1, 7, 6], [6, 1, 7], [6, 7, 1], [7, 1, 6], [7, 6, 1] ]
[1, 6, 8] --> [ [1, 6, 8], [1, 8, 6], [6, 1, 8], [6, 8, 1], [8, 1, 6], [8, 6, 1] ]
[1, 7, 8] --> [ [1, 7, 8], [1, 8, 7], [7, 1, 8], [7, 8, 1], [8, 1, 7], [8, 7, 1] ]
[2, 3, 4] --> [ [2, 3, 4], [2, 4, 3], [3, 2, 4], [3, 4, 2], [4, 2, 3], [4, 3, 2] ]
[2, 3, 5] --> [ [2, 3, 5], [2, 5, 3], [3, 2, 5], [3, 5, 2], [5, 2, 3], [5, 3, 2] ]
[2, 3, 6] --> [ [2, 3, 6], [2, 6, 3], [3, 2, 6], [3, 6, 2], [6, 2, 3], [6, 3, 2] ]
[2, 3, 7] --> [ [2, 3, 7], [2, 7, 3], [3, 2, 7], [3, 7, 2], [7, 2, 3], [7, 3, 2] ]
[2, 3, 8] --> [ [2, 3, 8], [2, 8, 3], [3, 2, 8], [3, 8, 2], [8, 2, 3], [8, 3, 2] ]
[2, 4, 5] --> [ [2, 4, 5], [2, 5, 4], [4, 2, 5], [4, 5, 2], [5, 2, 4], [5, 4, 2] ]
[2, 4, 6] --> [ [2, 4, 6], [2, 6, 4], [4, 2, 6], [4, 6, 2], [6, 2, 4], [6, 4, 2] ]
[2, 4, 7] --> [ [2, 4, 7], [2, 7, 4], [4, 2, 7], [4, 7, 2], [7, 2, 4], [7, 4, 2] ]
[2, 4, 8] --> [ [2, 4, 8], [2, 8, 4], [4, 2, 8], [4, 8, 2], [8, 2, 4], [8, 4, 2] ]
[2, 5, 6] --> [ [2, 5, 6], [2, 6, 5], [5, 2, 6], [5, 6, 2], [6, 2, 5], [6, 5, 2] ]
[2, 5, 7] --> [ [2, 5, 7], [2, 7, 5], [5, 2, 7], [5, 7, 2], [7, 2, 5], [7, 5, 2] ]
[2, 5, 8] --> [ [2, 5, 8], [2, 8, 5], [5, 2, 8], [5, 8, 2], [8, 2, 5], [8, 5, 2] ]
[2, 6, 7] --> [ [2, 6, 7], [2, 7, 6], [6, 2, 7], [6, 7, 2], [7, 2, 6], [7, 6, 2] ]
[2, 6, 8] --> [ [2, 6, 8], [2, 8, 6], [6, 2, 8], [6, 8, 2], [8, 2, 6], [8, 6, 2] ]
[2, 7, 8] --> [ [2, 7, 8], [2, 8, 7], [7, 2, 8], [7, 8, 2], [8, 2, 7], [8, 7, 2] ]
[3, 4, 5] --> [ [3, 4, 5], [3, 5, 4], [4, 3, 5], [4, 5, 3], [5, 3, 4], [5, 4, 3] ]
[3, 4, 6] --> [ [3, 4, 6], [3, 6, 4], [4, 3, 6], [4, 6, 3], [6, 3, 4], [6, 4, 3] ]
[3, 4, 7] --> [ [3, 4, 7], [3, 7, 4], [4, 3, 7], [4, 7, 3], [7, 3, 4], [7, 4, 3] ]
[3, 4, 8] --> [ [3, 4, 8], [3, 8, 4], [4, 3, 8], [4, 8, 3], [8, 3, 4], [8, 4, 3] ]
[3, 5, 6] --> [ [3, 5, 6], [3, 6, 5], [5, 3, 6], [5, 6, 3], [6, 3, 5], [6, 5, 3] ]
[3, 5, 7] --> [ [3, 5, 7], [3, 7, 5], [5, 3, 7], [5, 7, 3], [7, 3, 5], [7, 5, 3] ]
[3, 5, 8] --> [ [3, 5, 8], [3, 8, 5], [5, 3, 8], [5, 8, 3], [8, 3, 5], [8, 5, 3] ]
[3, 6, 7] --> [ [3, 6, 7], [3, 7, 6], [6, 3, 7], [6, 7, 3], [7, 3, 6], [7, 6, 3] ]
[3, 6, 8] --> [ [3, 6, 8], [3, 8, 6], [6, 3, 8], [6, 8, 3], [8, 3, 6], [8, 6, 3] ]
[3, 7, 8] --> [ [3, 7, 8], [3, 8, 7], [7, 3, 8], [7, 8, 3], [8, 3, 7], [8, 7, 3] ]
[4, 5, 6] --> [ [4, 5, 6], [4, 6, 5], [5, 4, 6], [5, 6, 4], [6, 4, 5], [6, 5, 4] ]
[4, 5, 7] --> [ [4, 5, 7], [4, 7, 5], [5, 4, 7], [5, 7, 4], [7, 4, 5], [7, 5, 4] ]
[4, 5, 8] --> [ [4, 5, 8], [4, 8, 5], [5, 4, 8], [5, 8, 4], [8, 4, 5], [8, 5, 4] ]
[4, 6, 7] --> [ [4, 6, 7], [4, 7, 6], [6, 4, 7], [6, 7, 4], [7, 4, 6], [7, 6, 4] ]
[4, 6, 8] --> [ [4, 6, 8], [4, 8, 6], [6, 4, 8], [6, 8, 4], [8, 4, 6], [8, 6, 4] ]
[4, 7, 8] --> [ [4, 7, 8], [4, 8, 7], [7, 4, 8], [7, 8, 4], [8, 4, 7], [8, 7, 4] ]
[5, 6, 7] --> [ [5, 6, 7], [5, 7, 6], [6, 5, 7], [6, 7, 5], [7, 5, 6], [7, 6, 5] ]
[5, 6, 8] --> [ [5, 6, 8], [5, 8, 6], [6, 5, 8], [6, 8, 5], [8, 5, 6], [8, 6, 5] ]
[5, 7, 8] --> [ [5, 7, 8], [5, 8, 7], [7, 5, 8], [7, 8, 5], [8, 5, 7], [8, 7, 5] ]
[6, 7, 8] --> [ [6, 7, 8], [6, 8, 7], [7, 6, 8], [7, 8, 6], [8, 6, 7], [8, 7, 6] ]
 */
