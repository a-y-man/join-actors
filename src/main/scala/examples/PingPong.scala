package join_patterns.examples

import actor.*
import com.typesafe.scalalogging.*
import join_patterns.*
import org.scalacheck.*
import org.scalatest.run

import java.util.concurrent.TimeUnit
import scala.compiletime.ops.int
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.*

type Ponger = ActorRef[Ping | Done]
type Pinger = ActorRef[Pong | Done]

sealed trait PingPong
case class Ping(ref: Pinger, hits: Int) extends PingPong
case class Pong(ref: Ponger, hits: Int) extends PingPong
case class Done(hits: Int)              extends PingPong

def pingPongExample(maxHits: Int = 100, algorithm: MatchingAlgorithm) =
  val logger = Logger("PingPong")

  val pingActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (y: PingPong, pingRef: Pinger) =>
        y match
          case Pong(pongRef, x) =>
            // logger.debug(s"x = $x")
            // def square(x: Int): Int = x * x + x + maxHits
            if x < maxHits then
              // val x: Int = square(maxHits + x)
              // logger.debug(s"x_squared = $x")
              // val x: Int = x + x + x + maxHits
              logger.debug(s"Ponged by $pongRef --- ping count: $x")
              pongRef ! Ping(pingRef, x + 1)
              Next()
            else
              // val x = 42
              // logger.debug(s"x' = $maxHits")
              pongRef ! Done(x)
              pingRef ! Done(x)
              Next()
          case Done(x) =>
            logger.debug(s"Final count: $x")
            Stop(x)
      }(algorithm)
    }

  val pongActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (y: PingPong, pongRef: Ponger) =>
        y match
          case Ping(pingRef, x) =>
            // logger.debug(s"x = $x")
            if x < maxHits then
              // val x = 42
              // logger.debug(s"x = $x")
              logger.debug(s"Pinged by $pingRef --- pong count: $x")
              // logger.debug(s"maxHits: $maxHits")
              pingRef ! Pong(pongRef, x + 1)
              Next()
            else
              pingRef ! Done(x)
              pongRef ! Done(x)
              Next()
          case Done(x) =>
            logger.debug(s"Final count: $x")
            Stop(x)
      }(algorithm)
    }

  val (futResult1, pinger) = pingActor.start()
  val (futResult2, ponger) = pongActor.start()

  val results = Future.sequence(Seq(futResult1, futResult2))

  ponger ! Ping(pinger, 0)

  val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

  finalResult.onComplete(printResult)
