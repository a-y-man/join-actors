package join_patterns.examples

import actor.*
import join_patterns.*
import org.scalacheck.*
import org.scalatest.run

import java.util.concurrent.TimeUnit
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
  val pingActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receiveOld { (y: PingPong, pingRef: Pinger) =>
        y match
          case Pong(pongRef, x) =>
            if x < maxHits then
              pongRef ! Ping(pingRef, x + 1)
              Continue()
            else
              pongRef ! Done(x)
              pingRef ! Done(x)
              Continue()
          case Done(x) =>
            Stop(x)
      }(algorithm)
    }

  val pongActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receiveOld { (y: PingPong, pongRef: Ponger) =>
        y match
          case Ping(pingRef, x) =>
            if x < maxHits then
              pingRef ! Pong(pongRef, x + 1)
              Continue()
            else
              pingRef ! Done(x)
              pongRef ! Done(x)
              Continue()
          case Done(x) =>
            Stop(x)
      }(algorithm)
    }

  val (futResult1, pinger) = pingActor.start()
  val (futResult2, ponger) = pongActor.start()

  val results = Future.sequence(Seq(futResult1, futResult2))

  ponger ! Ping(pinger, 0)

  val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

  finalResult.onComplete(printResult)
