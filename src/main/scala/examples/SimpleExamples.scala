package join_patterns.examples
import actor.*
import join_patterns.*
import org.scalacheck.*

import java.util.concurrent.TimeUnit
import scala.compiletime.ops.int
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
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

// def demo(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val actor = Actor[Msg, Unit](
//     receive { (msg: Msg, _: ActorRef[Msg]) =>
//       msg match
//         case (A(), B(), C()) => Stop(println(s"I've received 3 messages: A, B and C :)"))
//         case D(n) if n > 0 =>
//           Stop(println(s"I've received one message with the payload ${n} :)"))
//         case E(n) if n != n => Stop(println(s"I cannot happen :("))
//         case (F(a), E(b)) if (a + b == 42) =>
//           Stop(println(s"I've received 2 messages with the same payload :)"))
//     }(algorithm)
//   )
//   val (futureResult, actorRef) = actor.start()

//   val q = List[Msg](E(21), F(21))
//   // val q = List[Msg](A(), B(), C())
//   // val q = List[Msg](D(42))
//   // val q = List[Msg](E(2))

//   q.foreach(actorRef ! _)

//   println(s"Q = ${q.zipWithIndex}")
//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test01(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val actor = Actor[Msg, Unit](receive { (y: Msg, _: ActorRef[Msg]) =>
//     y match
//       case (D(x), E(y), F(z)) =>
//         Stop(println(s"Case 00: x = $x, y = $y, z = $z"))
//       case (D(x), F(z), E(y)) =>
//         Stop(println(s"Case 01: x = $x, y = $y, z = $z"))
//       case (E(y), D(x), F(z)) =>
//         Stop(println(s"Case 02: x = $x, y = $y, z = $z"))
//       case (E(y), F(z), D(x)) =>
//         Stop(println(s"Case 03: x = $x, y = $y, z = $z"))
//       case (F(z), D(x), E(y)) =>
//         Stop(println(s"Case 04: x = $x, y = $y, z = $z"))
//       case (F(z), E(y), D(x)) =>
//         Stop(println(s"Case 05: x = $x, y = $y, z = $z"))
//   }(algorithm))
//   val (futureResult, actorRef) = actor.start()
//   val q                        = List[Msg](D(3), F(2), E(1), A(), B(), C())

//   q.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")
//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test02(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val q = List.fill(9)(A())

//   val actor = Actor[Msg, Unit](
//     receive { (y: Msg, _: ActorRef[Msg]) =>
//       y match
//         case (A(), A(), A(), A(), A(), A(), A(), A(), A()) => Stop(println("Match!"))
//     }(algorithm)
//   )
//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")
//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test03(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")
//   val i: Int                 = 0;
//   val m                      = 0
//   val isZero: Int => Boolean = (n: Int) => n == 0

//   val q = List[Msg](E(2), F(2), E(42))

//   val actor = Actor[Msg, Int](
//     receive { (y: Msg, _: ActorRef[Msg]) =>
//       y match
//         case (E(m), E(n)) if n == 2 && m == 42 =>
//           { val z = "hi"; println(z) }; Stop(n + 1)
//         case (A(), B(), A(), E(n)) if n == 2 => Stop(500 * n)
//         case (B(), A(), B(), E(n)) if n == 2 => Stop(600 * n)
//     }(algorithm)
//   )

//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")
//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test04(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val i: Int                 = 0;
//   val m                      = 0
//   val isZero: Int => Boolean = (n: Int) => n == 0

//   val matcher: Matcher[Msg, Result[Unit]] = receive { (y: Msg, _: ActorRef[Msg]) =>
//     y match
//       case (E(m), F(n), E(o)) => Stop(println(s"E(m = $m), F(n = $n), E(o = $o)"))
//   }(algorithm)

//   val q  = List[Msg](E(4), F(2), E(1))
//   val q_ = List[Msg](A(), B(), A())

//   val actor                    = Actor[Msg, Unit](matcher)
//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)
//   // q_.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")
//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test05(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val matcher: Matcher[Msg, Result[String]] =
//     receive { (y: Msg, _: ActorRef[Msg]) =>
//       y match
//         case (
//               E(a),
//               E(b),
//               E(c),
//               E(d),
//               E(e),
//               E(f),
//               E(g),
//               E(h),
//               E(i),
//               E(j)
//             )
//             if a == 10 && b == 9 && c == 8 && d == 7 && e == 6 && f == 5 && g == 4 && h == 3 && i == 2 && j == 1 =>
//           Stop("Match!")
//     }(algorithm)

//   val q    = List[Msg](E(1), E(2), E(3), E(4), E(5), E(6), E(7), E(8), E(9), E(10))
//   val revQ = q.reverse

//   val actor                    = Actor[Msg, String](matcher)
//   val (futureResult, actorRef) = actor.start()

//   // q.foreach(actorRef ! _)
//   revQ.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")

//   val result = Await.result(futureResult, Duration(5, "minutes"))
//   println(result)
//   println("\n======================================================\n\n")

// def test06(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")
//   val expected = 42
//   val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg, self: ActorRef[Msg]) =>
//     y match
//       case (F(i0), E(i1)) if i0 == i1 =>
//         println(s"${self}")
//         Stop(expected)
//       case (F(i0), G(i1, s1, i2, b)) if i0 == i1 && s1 == s1 && b =>
//         println(s"${self}")
//         Stop(expected + 1)
//   }(algorithm)
//   val q = List[Msg](
//     B(),
//     A(),
//     F(4),
//     G(1, "G", 1, false),
//     B(),
//     E(1),
//     E(2),
//     E(3),
//     E(4),
//     E(5),
//     E(42),
//     G(42, "G", 1, true),
//     F(42)
//   )

//   val actor = Actor[Msg, Int](matcher)

//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)
//   println(s"Q =  ${q.zipWithIndex}")

//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test07(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")
//   val expected = 42
//   val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg, _: ActorRef[Msg]) =>
//     y match
//       case (F(i0), E(i1), F(i2)) if i0 == i1 && i1 == i2 =>
//         Stop(expected)
//       case F(a) => Stop(expected * a)
//   }(algorithm)

//   val q = List[Msg](F(4), E(4), F(4))

//   val actor                    = Actor[Msg, Int](matcher)
//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")
//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def test08(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val q = List[Msg](E(1), E(2), E(3), E(4), E(5), E(6), E(7), E(8), E(9), E(10), E(11), E(12))

//   val actor = Actor[Msg, Int] {
//     receive { (msg: Msg, _: ActorRef[Msg]) =>
//       msg match
//         case (E(a), E(b), E(c)) if a == 3 && b == 2 && c == 1    => Next()
//         case (E(a), E(b), E(c)) if a == 6 && b == 5 && c == 4    => Next()
//         case (E(a), E(b), E(c)) if a == 9 && b == 8 && c == 7    => Next()
//         case (E(a), E(b), E(c)) if a == 12 && b == 11 && c == 10 => Stop(a * b * c)
//     }(algorithm)
//   }
//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)

//   println(s"Q =  ${q.zipWithIndex}")

//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

// def randomMsgTesting(algorithm: MatchingAlgorithm): Unit =
//   println(s"Using ${algorithm}\n\n")

//   val matcher: Matcher[Msg, Result[Unit]] = receive { (msg: Msg, _: ActorRef[Msg]) =>
//     msg match
//       case (A(), B(), C()) => Stop(println(s"I've received 3 messages: A, B and C :)"))
//       case (D(n), E(m), F(o)) if n < m && m < o =>
//         Stop(println(s"I've received 3 messages: D, E and F :)"))
//       case (E(x), F(a), G(b, c, d, e)) if x >= a && a <= b && d <= c.length =>
//         Stop(println(s"I've received 3 messages: E, F and G :)"))
//   }(algorithm)

//   val msgsForCase1 = List[Msg](A(), B(), C())
//   val msgsForCase2 = List[Msg](D(1), E(2), F(3))
//   val msgsForCase3 = List[Msg](E(3), F(2), G(3, "G", 1, true))

//   val q = GenerateRandomMsgs.genRandomMsgs(10000)

//   val actor                    = Actor[Msg, Unit](matcher)
//   val (futureResult, actorRef) = actor.start()

//   q.foreach(actorRef ! _)
//   Random.nextInt(3) match
//     case 0 => msgsForCase1.foreach(actorRef ! _)
//     case 1 => msgsForCase2.foreach(actorRef ! _)
//     case 2 => msgsForCase3.foreach(actorRef ! _)

//   // println(s"Q =  ${q.zipWithIndex}")

//   val result = Await.ready(futureResult, Duration(1, "minutes"))
//   result.onComplete(printResult)

//   println("\n======================================================\n\n")

def nwptExample(algorithm: MatchingAlgorithm): Unit =

  sealed trait Message
  case class Buy(bN: String, bID: String, bA: Int)  extends Message
  case class Sell(sN: String, sID: String, sA: Int) extends Message

  val tradingSystemActor = Actor[Message, Unit] {
    receive { (msg: Message, systemActor: ActorRef[Message]) =>
      msg match
        case (Buy(bN, bID, bA), Sell(sN, sID, sA)) if sA >= bA =>
          Stop(println(s"${sA} >= ${bA}"))
        case (
              Sell(sN1, sID1, sA1),
              Sell(sN2, sID2, sA2),
              Buy(bN, bID, bA)
            ) if (sA1 + sA2) >= bA =>
          Stop(println(s"${sA1} + ${sA2} >= ${bA}"))
    }(algorithm)
  }

  val (futureResult, actorRef) = tradingSystemActor.start()

  val msgs = List(Sell("A", "1", 10), Sell("A", "1", 10), Sell("A", "1", 20), Buy("A", "1", 20))

  msgs.foreach(actorRef ! _)

  println(s"Q =  ${msgs.zipWithIndex}")

  val result = Await.ready(futureResult, Duration(1, "minutes"))

  result.onComplete(printResult)

  println("\n======================================================\n\n")
