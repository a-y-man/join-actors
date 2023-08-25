package join_patterns
import actor._
import join_patterns._

import scala.util.*

import concurrent.ExecutionContext.Implicits.global
sealed trait Msg
case class A()                                      extends Msg
case class B()                                      extends Msg
case class C()                                      extends Msg
case class D(a: Int)                                extends Msg
case class E(a: Int)                                extends Msg
case class F(a: Int)                                extends Msg
case class G(b: Int, a: String, c: Int, d: Boolean) extends Msg

def printResult[A](result: Try[A]): Unit = result match {
  case Failure(exception) => println("Failed with: " + exception.getMessage)
  case Success(number)    => println("Succeed with: " + number)
}

def demo(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[Unit]] = receive { (msg: Msg) =>
    msg match
      case (A(), B(), C()) => Stop(println(s"I've received 3 messages: A, B and C :)"))
      case D(n: Int) if n > 0 =>
        Stop(println(s"I've received one message with the payload ${n} :)"))
      case E(n: Int) if n != n => Stop(println(s"I cannot happen :("))
      case (F(a: Int), E(b: Int)) if (a + b == 42) =>
        Stop(println(s"I've received 2 messages with the same payload :)"))
  }(algorithm)

  val q = List[Msg](E(21), F(21))
  // val q = List[Msg](A(), B(), C())
  // val q = List[Msg](D(42))
  // val q = List[Msg](E(2))

  val actor                    = Actor_[Msg, Unit] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q = ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test01(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[Unit]] = receive { (y: Msg) =>
    y match
      case (D(x: Int), E(y: Int), F(z: Int)) =>
        Stop(println(s"Case 00: x = ${x}, y = ${y}, z = ${z}"))
      case (D(x: Int), F(z: Int), E(y: Int)) =>
        Stop(println(s"Case 01: x = ${x}, y = ${y}, z = ${z}"))
      case (E(y: Int), D(x: Int), F(z: Int)) =>
        Stop(println(s"Case 02: x = ${x}, y = ${y}, z = ${z}"))
      case (E(y: Int), F(z: Int), D(x: Int)) =>
        Stop(println(s"Case 03: x = ${x}, y = ${y}, z = ${z}"))
      case (F(z: Int), D(x: Int), E(y: Int)) =>
        Stop(println(s"Case 04: x = ${x}, y = ${y}, z = ${z}"))
      case (F(z: Int), E(y: Int), D(x: Int)) =>
        Stop(println(s"Case 05: x = ${x}, y = ${y}, z = ${z}"))
  }(algorithm)

  val q                        = List[Msg](D(3), F(2), E(1), A(), B(), C())
  val actor                    = Actor_[Msg, Unit] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test02(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[Unit]] =
    receive { (y: Msg) =>
      y match
        case (A(), A(), A(), A(), A(), A(), A(), A(), A()) => Stop(println("Match!"))
    }(algorithm)

  val q = List.fill(9)(A())

  val actor                    = Actor_[Msg, Unit] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test03(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0

  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (E(m: Int), E(n: Int)) if n == 2 && m == 42 => {
        { val z = "hi"; println(z) }; Stop(n + 1)
      }
      case (A(), B(), A(), E(n: Int)) if n == 2 => Stop(500 * n)
      case (B(), A(), B(), E(n: Int)) if n == 2 => Stop(600 * n)
  }(algorithm)

  val q = List[Msg](E(4), F(2), E(1))

  val actor = Actor_[Msg, Int] { matcher }

  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test04(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0

  val matcher: Matcher[Msg, Result[Unit]] = receive { (y: Msg) =>
    y match
      case (E(m: Int), F(n: Int), E(o: Int)) => {
        { val z = "E(m: Int), F(n: Int), E(o: Int)"; Stop(println(z)) }
      }
  }(algorithm)

  val q  = List[Msg](E(4), F(2), E(1))
  val q_ = List[Msg](A(), B(), A())

  val actor                    = Actor_[Msg, Unit] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)
  // q_.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test05(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[String]] =
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
          Stop("Match!")
    }(algorithm)

  val q    = List[Msg](E(1), E(2), E(3), E(4), E(5), E(6), E(7), E(8), E(9), E(10))
  val revQ = q.reverse

  val actor                    = Actor_[Msg, String] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)
  // revQ.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test06(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val result = 42
  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (F(i0: Int), E(i1: Int)) if i0 == i1 =>
        Stop(result)
      case (F(i0: Int), G(i1: Int, s1: String, i2: Int, b: Boolean)) if i0 == i1 && s1 == s1 && b =>
        Stop(result + 1)
  }(algorithm)
  val q = List[Msg](
    B(),
    A(),
    F(4),
    G(1, "G", 1, false),
    B(),
    E(1),
    E(2),
    E(3),
    E(4),
    E(5),
    E(42),
    G(42, "G", 1, true),
    F(42)
  )

  val actor = Actor_[Msg, Int] { matcher }

  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)
  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test07(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val result = 42
  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (F(i0: Int), E(i1: Int), F(i2: Int)) if i0 == i1 && i1 == i2 =>
        Stop(result)
      case F(a: Int) => Stop(result * a)
  }(algorithm)

  val q = List[Msg](F(4), E(4), F(4))

  val actor                    = Actor_[Msg, Int] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")

def test08(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 3 && b == 2 && c == 1    => Stop(a * b * c)
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 4 && b == 5 && c == 6    => Stop(a * b * c)
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 7 && b == 8 && c == 9    => Stop(a * b * c)
      case (E(a: Int), E(b: Int), E(c: Int)) if a == 16 && b == 32 && c == 64 => Stop(a * b * c)
  }(algorithm)

  val q = List[Msg](E(0), E(0), E(0), E(0), E(0), E(0), E(64), E(32), E(16))

  val actor                    = Actor_[Msg, Int] { matcher }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  futureResult.onComplete(printResult)
  println("\n======================================================\n\n")
