package test.scenario.pingPong

import join_actors.api.MatchingAlgorithm.*
import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import test.classes.pingPong.*

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Random

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(Executors.newVirtualThreadPerTaskExecutor())

val matchingAlgos = Table(
  "MatchingAlgorithm",
  BruteForceAlgorithm,
  StatefulTreeBasedAlgorithm,
  MutableStatefulAlgorithm,
  LazyMutableAlgorithm,
  WhileLazyAlgorithm,
  FilteringWhileAlgorithm,
  WhileEagerAlgorithm,
  EagerParallelAlgorithm(2),
  LazyParallelAlgorithm(2),
  FilteringParallelAlgorithm(2),
  ArrayWhileAlgorithm,
  ArrayParallelAlgorithm(2)
)

class PingPongTest extends AnyFunSuite:
  test(s"Fixed number of iterations") {
    val maxHits = 100

    forAll(matchingAlgos) { algorithm =>
      val (pingActor, pongActor) = pingPonger(maxHits, algorithm)
      val (result1, pinger)      = pingActor.start()
      val (result2, ponger)      = pongActor.start()

      val results = Future.sequence(Seq(result1, result2))

      ponger ! Ping(pinger, 0)

      val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

      finalResult map { results => assert(results forall (_ == maxHits)) }
    }
  }

  test("Random number of iterations") {
    val maxHits = Random.nextInt(100)

    forAll(matchingAlgos) { algorithm =>
      val (pingActor, pongActor) = pingPonger(maxHits, algorithm)
      val (result1, pinger)      = pingActor.start()
      val (result2, ponger)      = pongActor.start()

      val results = Future.sequence(Seq(result1, result2))

      ponger ! Ping(pinger, 0)

      val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

      finalResult map { results => assert(results forall (_ == maxHits)) }
    }
  }
