package test

import actor.*
import join_patterns.*
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.LinkedTransferQueue
import scala.collection.immutable.List
import scala.concurrent.Await
import scala.util.Random

abstract class UnitTests extends AnyFunSuite:
  sealed trait Msg
  case class A()                                      extends Msg
  case class B(n: Int)                                extends Msg
  case class C(n: String)                             extends Msg
  case class D()                                      extends Msg
  case class E()                                      extends Msg
  case class F(b: Int, a: String)                     extends Msg
  case class G(b: Int, a: String, c: Int, d: Boolean) extends Msg

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

class SingletonPatterns extends UnitTests:
  test("Single Empty Message, no Predicate") {
    val expected = Random.nextInt
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case A() => Stop(expected)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! A()

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)
    assert(actual == expected)
  }

  test("Single Empty Message, Predicate") {
    val expected = Random.nextInt
    val ifZero   = (i: Int) => i == 0
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case A() if ifZero(1) => Stop(expected + 1)
        case A() if ifZero(0) => Stop(expected)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! A()

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Single Message, One Int Member, no Predicate") {
    val expected = Random.nextInt
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case B(n: Int) => Stop(n)
    }(ALGORITHM)

    val actor                    = Actor_[Msg, Int] { matcher }
    val (futureResult, actorRef) = actor.start()

    actorRef ! B(expected)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Single Message, One Int Member, Predicate") {
    val expected = Random.nextInt
    val ifZero   = (i: Int) => i == 0
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case B(n: Int) if ifZero(1) => Stop(n + 1) // Always false
        case B(n: Int) if ifZero(0) => Stop(n)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! B(expected)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Single Message, One String Member, no Predicate") {
    val expected = "test"
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case C(n: String) => Stop(n)
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! C(expected)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Single Message, One String Member, Predicate") {
    val expected   = "test"
    val ifNotEmpty = (i: String) => !i.isEmpty
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case C(n: String) if ifNotEmpty("") =>
          Stop(n.appended(Random.alphanumeric.filter(_.isDigit).head))
        case C(n: String) if ifNotEmpty(n) => Stop(n)
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! C(expected)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Single Message, One Int and One String Members, no Predicate") {
    val rep      = Random.nextInt(5)
    val input    = "test "
    val expected = input.repeat(rep)
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case F(z: Int, c: String) => Stop(c.repeat(z))
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! F(rep, input)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Single Message, One Int and One String Members, Predicate") {
    val expected               = "test "
    val rep                    = Random.nextInt(5)
    val isZero: Int => Boolean = (n: Int) => n == 0
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case F(z: Int, c: String) if isZero(z) => Stop(c)
        case F(z: Int, c: String)              => Stop(c.repeat(z))
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! F(rep, expected)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(if rep == 0 then actual == expected else actual == expected.repeat(rep))
  }

class CompositePatterns extends UnitTests:
  test("Multiple Empty Messages, no Predicate") {
    val expected = Random.nextInt
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (D(), A(), E()) => Stop(expected)
        case (A(), D())      => Stop(expected + 1)
        case (D(), E())      => Stop(expected + 2)
        case D()             => Stop(expected + 3)
        case E()             => Stop(expected + 4)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! A()
    actorRef ! D()
    actorRef ! E()

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected + 1)
  }

  test("One Tupled Empty Message, no Predicate") {
    val expected = Random.nextInt
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (A()) => Stop(expected)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! A()

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Multiple Empty Messages, Predicate") {
    val expected               = Random.nextInt
    val isZero: Int => Boolean = (n: Int) => n == 0
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (A(), D(), E()) if isZero(0) => Stop(expected + 1)
        case (A(), D(), E())              => Stop(expected)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! A()
    actorRef ! D()
    actorRef ! E()

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected + 1)
  }

  test("Multiple Messages of the same Class, One Int Member, no Predicate") {
    val (i0, i1) = (Random.nextInt, Random.nextInt)
    val expected = i0
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (B(i0: Int), B(i1: Int)) => Stop(i0 + i1)
        case B(i: Int) =>
          Stop(i) // This will always matches because there is no predicate on both cases.
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! B(i0)
    actorRef ! B(i1)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Multiple Messages, One Int and One String Members, no Predicate") {
    val expected = "test "
    val rep      = Random.nextInt(3)
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case (F(i0: Int, s: String), D(), B(i1: Int)) => Stop(s.repeat(i0 + i1))
        case D()                                      => Stop(expected)
        case B(i: Int)                                => Stop(rep.toString)
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! F(rep, expected)
    actorRef ! D() // This will cause the second case to be matched first.
    actorRef ! B(rep)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)
    assert(actual == expected)
  }

  test("Multiple Messages, One Int and One String Members, Predicate") {
    val expected                   = "Hello World"
    val rep                        = Random.nextInt(3)
    val isEmpty: String => Boolean = (s: String) => s.isEmpty
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case (F(i0: Int, s: String), D(), B(i1: Int)) if isEmpty(s) => Stop("Hello World")
        case (F(i0: Int, s: String), D(), B(i1: Int)) if isEmpty(s) == false =>
          Stop(("Hello " + s).repeat(i0 + i1))
        case B(i: Int) => Stop(rep.toString)
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! F(rep, "")
    actorRef ! D()
    actorRef ! B(rep)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Multiple Messages with irrelevant message types for the join-patterns") {
    val expected = Random.nextInt
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (D(), A(), E()) => Stop(expected)
        case (A(), E())      => Stop(expected + 1)
        case (D(), E())      => Stop(expected + 2)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! B(1)
    actorRef ! B(1)
    actorRef ! A()
    actorRef ! B(1)
    actorRef ! B(1)
    actorRef ! B(1)
    actorRef ! C("")
    actorRef ! B(1)
    actorRef ! D()
    actorRef ! B(1)
    actorRef ! E()
    actorRef ! B(1)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected + 1)
  }

  test("Multiple Messages with irrelevant message types for the join-patterns, with guards") {
    val expected = Random.nextInt
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (F(i0: Int, s: String), B(i1: Int)) if i0 == i1 => Stop(expected)
        case (F(i0: Int, s1: String), G(i1: Int, s2: String, i2: Int, b: Boolean))
            if i0 == i1 && s1 == s2 && b =>
          Stop(expected + 1)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! B(-1)
    actorRef ! B(0)
    actorRef ! A()
    actorRef ! B(2)
    actorRef ! B(3)
    actorRef ! B(4)
    actorRef ! C("")
    actorRef ! B(5)
    actorRef ! F(1, "F")
    actorRef ! D()
    actorRef ! G(1, "G", 1, false)
    actorRef ! E()
    actorRef ! B(10)
    actorRef ! F(42, "G")
    actorRef ! G(42, "G", 1, true)
    actorRef ! B(1)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected + 1)
  }

  test("Multiple Messages of the same Class, One Int Member, Predicate") {
    val (result0, result1) = (Random.nextInt(Int.MaxValue - 1) + 1, Random.nextInt)
    val expected           = result0
    val ifNotZero          = (i: Int) => i != 0
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case (B(i0: Int), B(i1: Int), A()) if ifNotZero(result0) => Stop(i0 + i1)
        case (B(i0: Int), B(i1: Int), A())                       => Stop(i0)
        case B(i: Int)                                           => Stop(i)
    }(ALGORITHM)
    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! B(result0)
    actorRef ! A()
    actorRef ! B(result1)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Wildcard field names, no Predicate") {
    val (result0, result1, result2) = (Random.nextInt, Random.nextInt.toString, Random.nextInt)
    val expected                    = result1 + result2
    val matcher: Matcher[Msg, Result[String]] = receive { (y: Msg) =>
      y match
        case G(_: Int, y: String, z: Int, _: Boolean) => Stop(y + z)
    }(ALGORITHM)

    val actor = Actor_[Msg, String] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! G(result0, result1, result2, false)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Wildcard field names, Predicate") {
    val (result0, result1, result2, result3) =
      (Random.nextInt, Random.nextInt.toString, Random.nextInt, Random.nextBoolean)
    val expected = if result3 then result2 else result0
    val is: Boolean => Boolean =
      (boolean: Boolean) => boolean
    val matcher: Matcher[Msg, Result[Int]] = receive { (y: Msg) =>
      y match
        case G(_: Int, _: String, z: Int, b: Boolean) if is(b)  => Stop(z)
        case G(y: Int, _: String, _: Int, b: Boolean) if is(!b) => Stop(y)
    }(ALGORITHM)

    val actor = Actor_[Msg, Int] { matcher }

    val (futureResult, actorRef) = actor.start()

    actorRef ! G(result0, result1, result2, result3)

    val actual = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)

    assert(actual == expected)
  }

  test("Consume all messages in mailbox until a match with a Stop") {
    val matcher: Matcher[Msg, Result[Int]] = receive { (msg: Msg) =>
      msg match
        case (B(a: Int), B(b: Int), B(c: Int)) if a == 3 && b == 2 && c == 1    => Next()
        case (B(a: Int), B(b: Int), B(c: Int)) if a == 6 && b == 5 && c == 4    => Next()
        case (B(a: Int), B(b: Int), B(c: Int)) if a == 9 && b == 8 && c == 7    => Next()
        case (B(a: Int), B(b: Int), B(c: Int)) if a == 12 && b == 11 && c == 10 => Stop(a * b * c)
    }(ALGORITHM)

    val msgs = List[Msg](B(1), B(2), B(3), B(4), B(5), B(6), B(7), B(8), B(9), B(10), B(11), B(12))

    val actor                    = Actor_[Msg, Int] { matcher }
    val (futureResult, actorRef) = actor.start()

    msgs.foreach(actorRef ! _)

    val actual   = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)
    val expected = 10 * 11 * 12

    assert(actual == expected)
  }

  test("An actor that sends irrelevant messages mixed with correct messages to a matcher") {
    val matcher: Matcher[Msg, Result[Int]] = receive { (msg: Msg) =>
      msg match
        case (B(a: Int), A(), B(b: Int)) if a == 3 && b == 2 => Stop(a + b)
    }(ALGORITHM)

    val msgs = List[Msg](D(), C(""), G(1, "", 1, false), F(1, ""), B(3), A(), B(2))

    val actor                    = Actor_[Msg, Int] { matcher }
    val (futureResult, actorRef) = actor.start()

    msgs.foreach(actorRef ! _)

    val actual   = Await.result(futureResult, scala.concurrent.duration.Duration.Inf)
    val expected = 5

    assert(actual == expected)
  }
