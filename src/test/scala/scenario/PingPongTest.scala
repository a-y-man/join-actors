package test.scenario.pingPong

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertions.*
import scala.util.Random
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import test.classes.pingPong.*
import join_patterns.MatchingAlgorithm
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import ExecutionContext.Implicits.global

class PingPongTest extends AnyFunSuite:
  test(s"Fixed number of iterations with ${MatchingAlgorithm.StatefulTreeBasedAlgorithm}") {
    val maxHits = 100

    val (pingActor, pongActor) = pingPonger(maxHits, MatchingAlgorithm.StatefulTreeBasedAlgorithm)
    val (result1, pinger)      = pingActor.start()
    val (result2, ponger)      = pongActor.start()

    val results = Future.sequence(Seq(result1, result2))

    ponger ! Ping(pinger, 0)

    val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

    finalResult map { results => assert(results forall (_ == maxHits)) }
  }

  test("Random number of iterations") {
    val maxHits = Random.nextInt(100)

    val (pingActor, pongActor) = pingPonger(maxHits, MatchingAlgorithm.StatefulTreeBasedAlgorithm)
    val (result1, pinger)      = pingActor.start()
    val (result2, ponger)      = pongActor.start()

    val results = Future.sequence(Seq(result1, result2))

    ponger ! Ping(pinger, 0)

    val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

    finalResult map { results => assert(results forall (_ == maxHits)) }
  }
