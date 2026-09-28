package test.lifecycle

import join_actors.api.*
import join_patterns.matching.Matcher
import join_patterns.types.JoinDefinition
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import test.utils.*

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.LinkedTransferQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

enum Msg:
  case A(x: Int)
  case B(x: Int)
  case Boom()
  case Done()

import Msg.*

/** Counts `close()` calls on the matchers it creates. */
class ClosingProbe(inner: MatcherFactory) extends MatcherFactory:
  val closed = AtomicInteger()
  def apply[M, T]: JoinDefinition[M, T] => Matcher[M, T] = jd =>
    val m = inner[M, T](jd)
    new Matcher[M, T]:
      def apply(q: LinkedTransferQueue[M])(self: ActorRef[M]): T = m(q)(self)
      override def close(): Unit =
        m.close()
        closed.incrementAndGet()

class ActorLifecycleTests extends AnyFunSuite:
  def throwingRhs(matcher: MatcherFactory) =
    Actor[Msg, Int] {
      receive { (_: ActorRef[Msg]) =>
        {
          case A(x) &:& B(y) if x == y => Continue
          case Boom() => throw IllegalStateException("boom")
          case Done() => Stop(0)
        }
      }(matcher)
    }

  test("an exception in a join pattern body fails the actor's future") {
    forAll(matchers) { matcher =>
      val (fut, ref) = throwingRhs(matcher).start()
      ref ! A(1)
      ref ! Boom()
      val e = intercept[IllegalStateException](Await.result(fut, 10.seconds))
      e.getMessage shouldBe "boom"
    }
  }

  test("an exception in a guard fails the actor's future") {
    forAll(matchers) { matcher =>
      val (fut, ref) = Actor[Msg, Int] {
        receive { (_: ActorRef[Msg]) =>
          {
            case A(x) &:& B(y) if x / (y - y) == 0 => Stop(x)
          }
        }(matcher)
      }.start()
      ref ! A(1)
      ref ! B(1)
      an[Exception] should be thrownBy Await.result(fut, 10.seconds)
    }
  }

  test("an exception in a SimpleActor handler fails its future") {
    val (fut, ref) = SimpleActor[Msg, Int] { _ =>
      { case Boom() => throw IllegalStateException("boom") }
    }.start()
    ref ! Boom()
    an[IllegalStateException] should be thrownBy Await.result(fut, 10.seconds)
  }

  test("the matcher is closed when the actor stops or fails") {
    forAll(matchers) { matcher =>
      val probe = ClosingProbe(matcher)

      val (stopped, stoppedRef) = throwingRhs(probe).start()
      stoppedRef ! Done()
      Await.result(stopped, 10.seconds) shouldBe 0

      val (failed, failedRef) = throwingRhs(probe).start()
      failedRef ! Boom()
      Await.ready(failed, 10.seconds)

      // close() runs right after the future completes, on the actor thread
      val deadline = 5.seconds.fromNow
      while probe.closed.get() < 2 && deadline.hasTimeLeft() do Thread.sleep(10)
      probe.closed.get() shouldBe 2
    }
  }

  test("a stopped actor releases the threads of its parallel matcher") {
    // LazyParallelMatcher evaluates guards on its own pool, so the guard can report that thread.
    val poolThreads = ConcurrentLinkedQueue[Thread]()
    def record(): Boolean =
      poolThreads.add(Thread.currentThread())
      true

    val (fut, ref) = Actor[Msg, Int] {
      receive { (_: ActorRef[Msg]) =>
        {
          case A(x) &:& B(y) if x == y && record() => Stop(x)
        }
      }(LazyParallelMatcher(2))
    }.start()
    ref ! A(1)
    ref ! B(1)
    Await.result(fut, 10.seconds) shouldBe 1

    val threads = poolThreads.asScala.toList.distinct
    threads should not be empty
    all(threads.map(_.getName)) should startWith("join-matcher-")
    for t <- threads do t.join(5000)
    threads.filter(_.isAlive) shouldBe empty
  }
