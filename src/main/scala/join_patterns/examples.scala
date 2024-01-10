package join_patterns
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

def printResult[A](result: Try[A]): Unit = result match
  case Failure(exception) => println("Failed with: " + exception.getMessage)
  case Success(number)    => println("Succeed with: " + number)

object GenerateRandomMsgs:
  // Set seed for the random generator
  // Random.setSeed(1234567890)

  private val genA: Gen[A] = Gen.const(A())
  private val genB: Gen[B] = Gen.const(B())
  private val genC: Gen[C] = Gen.const(C())
  private val genD: Gen[D] = Gen.choose(0, 100).map(D(_))
  private val genE: Gen[E] = Gen.choose(0, 100).map(E(_))
  private val genF: Gen[F] = Gen.choose(0, 100).map(F(_))
  private val genG: Gen[G] =
    for
      b <- Gen.choose(0, 100)
      a <- Gen.alphaStr
      c <- Gen.choose(0, 100)
      d <- Gen.oneOf(true, false)
    yield G(b, a, c, d)

  private val genMsg: Gen[Msg] = Gen.oneOf(genA, genB, genC, genD, genE, genF, genG)

  def genRandomMsgs(n: Int): List[Msg] =
    Gen.containerOfN[List, Msg](n, genMsg).sample.get

  def genWeightedRandomMsgs(n: Int, weights: List[Tuple2[Int, Gen[Msg]]]): List[Msg] =
    val genMsg = Gen.frequency(weights*)
    Gen.containerOfN[List, Msg](n, genMsg).sample.get

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

  val actor                    = Actor[Msg, Unit](matcher)
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q = ${q.zipWithIndex}")
  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

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
  val actor                    = Actor[Msg, Unit](matcher)
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def test02(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[Unit]] =
    receive { (y: Msg) =>
      y match
        case (A(), A(), A(), A(), A(), A(), A(), A(), A()) => Stop(println("Match!"))
    }(algorithm)

  val q = List.fill(9)(A())

  val actor                    = Actor[Msg, Unit](matcher)
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def test03(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0

  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (E(m: Int), E(n: Int)) if n == 2 && m == 42 =>
        { val z = "hi"; println(z) }; Stop(n + 1)
      case (A(), B(), A(), E(n: Int)) if n == 2 => Stop(500 * n)
      case (B(), A(), B(), E(n: Int)) if n == 2 => Stop(600 * n)
  }(algorithm)

  val q = List[Msg](E(2), F(2), E(42))

  val actor = Actor[Msg, Int](matcher)

  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def test04(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0

  val matcher: Matcher[Msg, Result[Unit]] = receive { (y: Msg) =>
    y match
      case (E(m: Int), F(n: Int), E(o: Int)) =>
        val z = "E(m: Int), F(n: Int), E(o: Int)"; Stop(println(z))
  }(algorithm)

  val q  = List[Msg](E(4), F(2), E(1))
  val q_ = List[Msg](A(), B(), A())

  val actor                    = Actor[Msg, Unit](matcher)
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)
  // q_.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

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

  val actor                    = Actor[Msg, String](matcher)
  val (futureResult, actorRef) = actor.start()

  // q.foreach(actorRef ! _)
  revQ.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")

  val result = Await.result(futureResult, Duration(5, "minutes"))
  println(result)
  println("\n======================================================\n\n")

def test06(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val expected = 42
  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (F(i0: Int), E(i1: Int)) if i0 == i1 =>
        Stop(expected)
      case (F(i0: Int), G(i1: Int, s1: String, i2: Int, b: Boolean)) if i0 == i1 && s1 == s1 && b =>
        Stop(expected + 1)
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

  val actor = Actor[Msg, Int](matcher)

  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)
  println(s"Q =  ${q.zipWithIndex}")

  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def test07(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")
  val expected = 42
  val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
    y match
      case (F(i0: Int), E(i1: Int), F(i2: Int)) if i0 == i1 && i1 == i2 =>
        Stop(expected)
      case F(a: Int) => Stop(expected * a)
  }(algorithm)

  val q = List[Msg](F(4), E(4), F(4))

  val actor                    = Actor[Msg, Int](matcher)
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")
  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def test08(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val q = List[Msg](E(1), E(2), E(3), E(4), E(5), E(6), E(7), E(8), E(9), E(10), E(11), E(12))

  val actor = Actor[Msg, Int] {
    receive { (msg: Msg) =>
      msg match
        case (E(a: Int), E(b: Int), E(c: Int)) if a == 3 && b == 2 && c == 1    => Next()
        case (E(a: Int), E(b: Int), E(c: Int)) if a == 6 && b == 5 && c == 4    => Next()
        case (E(a: Int), E(b: Int), E(c: Int)) if a == 9 && b == 8 && c == 7    => Next()
        case (E(a: Int), E(b: Int), E(c: Int)) if a == 12 && b == 11 && c == 10 => Stop(a * b * c)
    }(algorithm)
  }
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)

  println(s"Q =  ${q.zipWithIndex}")

  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def randomMsgTesting(algorithm: MatchingAlgorithm): Unit =
  println(s"Using ${algorithm}\n\n")

  val matcher: Matcher[Msg, Result[Unit]] = receive { (msg: Msg) =>
    msg match
      case (A(), B(), C()) => Stop(println(s"I've received 3 messages: A, B and C :)"))
      case (D(n: Int), E(m: Int), F(o: Int)) if n < m && m < o =>
        Stop(println(s"I've received 3 messages: D, E and F :)"))
      case (E(x: Int), F(a: Int), G(b: Int, c: String, d: Int, e: Boolean))
          if x >= a && a <= b && d <= c.length =>
        Stop(println(s"I've received 3 messages: E, F and G :)"))
  }(algorithm)

  val msgsForCase1 = List[Msg](A(), B(), C())
  val msgsForCase2 = List[Msg](D(1), E(2), F(3))
  val msgsForCase3 = List[Msg](E(3), F(2), G(3, "G", 1, true))

  val q = GenerateRandomMsgs.genRandomMsgs(10000)

  val actor                    = Actor[Msg, Unit](matcher)
  val (futureResult, actorRef) = actor.start()

  q.foreach(actorRef ! _)
  Random.nextInt(3) match
    case 0 => msgsForCase1.foreach(actorRef ! _)
    case 1 => msgsForCase2.foreach(actorRef ! _)
    case 2 => msgsForCase3.foreach(actorRef ! _)

  // println(s"Q =  ${q.zipWithIndex}")

  val result = Await.ready(futureResult, Duration(1, "minutes"))
  result.onComplete(printResult)

  println("\n======================================================\n\n")

def nwptExample(algorithm: MatchingAlgorithm): Unit =

  sealed trait Message // The types of the messages used in the system defined as an ADT
  case class Buy(bN: String, bID: String, bA: Int)  extends Message
  case class Sell(sN: String, sID: String, sA: Int) extends Message

  // def isAccepted(bN: String, bID: String, bA: Int, sN: String, sID: String, sA: Int): Boolean =
  //     (sA >= bA) && (sN == bN) && (sID != bID)

  // def isAcceptedSum(sN1: String, sID1: String, sA1: Int, sN2: String, sID2: String, sA2: Int, bN: String, bID: String, bA: Int): Boolean =
  //   (sA1 + sA2) >= bA && (sN1 == sN2) && (sID1 != sID2)
  //     (sA1 + sA2) >= bA && (sN1 == sN2) && (sID1 != sID2) &&

  val tradingSystemActor = Actor[Message, Unit] {
    receive { (msg: Message) =>
      msg match
        case (Buy(bN: String, bID: String, bA: Int), Sell(sN: String, sID: String, sA: Int))
            if (sA >= bA) =>
          Stop(println("Buy and Sell"))
        case (
              Sell(sN1: String, sID1: String, sA1: Int),
              Sell(sN2: String, sID2: String, sA2: Int),
              Buy(bN: String, bID: String, bA: Int)
            ) if ((sA1 + sA2) >= bA) =>
          Stop(println("Sell and Sell and Buy"))
    }(algorithm)
  }

  val (futureResult, actorRef) = tradingSystemActor.start()

  val msgs = List(Sell("A", "1", 10), Sell("A", "1", 10), Sell("A", "1", 20), Buy("A", "1", 20))

  msgs.foreach(actorRef ! _)

  println(s"Q =  ${msgs.zipWithIndex}")

  val result = Await.ready(futureResult, Duration(1, "minutes"))

  result.onComplete(printResult)

  println("\n======================================================\n\n")

object PingPong extends App:
  sealed trait PingPong
  case class Ping(ref: ActorRef[Ping], hits: Int) extends PingPong
  case class Pong(ref: ActorRef[Pong], hits: Int) extends PingPong
  case class StopMessage()                        extends PingPong
  case class StartMessage()                       extends PingPong

  type Ponger = ActorRef[PingPong]
  type Pinger = ActorRef[PingPong]

  val ALGORITHM = MatchingAlgorithm.BasicAlgorithm

  def pingActor() =
    var actor: Actor[PingPong, Unit] = null
    // var counter = 0
    // def incrementCounter(): Unit =
    //   counter += 1
    //   println(s"ping count: $counter")

    actor = Actor[PingPong, Unit](receive { (y: PingPong /*, self: Pinger (initially None)  */ ) =>
      y match
        // case StartMessage() =>
        //   incrementCounter()
        //   pingRef ! Ping(actor.self, counter)
        case Pong(pongRef: Ponger, x: Int) =>
          if x < 100000 then
            /*
            val pongRef : Ponger = ...
            val x : Int = ...
            val pingRef : Pinger = ...
             */
            println(s"Ponged by $pongRef --- ping count: $x")
            pongRef ! Ping(actor.self, x + 1)
            Next()
          else
            pongRef ! Ping(actor.self, x)
            Stop(println(s"Final count: $x"))
    }(ALGORITHM))

    actor

  def pongActor() =
    var actor: Actor[PingPong, Unit] = null

    actor = Actor[PingPong, Unit](receive { (x: PingPong /*, self: Pinger (initially None)  */ ) =>
      x match
        case Ping(pingRef: Pinger, x: Int) =>
          if x < 100000 then
            /*
            val pongRef : Ponger = ...
            val x : Int = ...
            val pingRef : Pinger = ...
             */
            // println(s"pong count: $x")
            println(s"I'm $actor.self ..Pinged by $pingRef --- pong count: $x")
            pingRef ! Pong(actor.self /* self */, x + 1)
            Next()
          else
            pingRef ! Pong(actor.self, x)
            Stop(println(s"Final count: $x"))
    }(ALGORITHM))

    actor

  def pingPonger() =
    val (futResult1, pinger) = pingActor().start()
    val (futResult2, ponger) = pongActor().start()

    val results = Future.sequence(Seq(futResult1, futResult2))

    pinger ! Pong(ponger, 0)

    val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

    finalResult.onComplete(printResult)

    println("\n======================================================\n\n")

  pingPonger()
