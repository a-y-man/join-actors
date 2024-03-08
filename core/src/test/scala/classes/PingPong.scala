package test.classes.pingPong

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

type Ponger = ActorRef[Ping | Done]
type Pinger = ActorRef[Pong | Done]

sealed trait PingPong
case class Ping(ref: Pinger, hits: Int) extends PingPong
case class Pong(ref: Ponger, hits: Int) extends PingPong
case class Done(hits: Int)              extends PingPong

def pingPonger(maxHits: Int = 100, algorithm: MatchingAlgorithm) =
  val pingActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (y: PingPong, pingRef: Pinger) =>
        y match
          case Pong(pongRef, x) =>
            if x < maxHits then
              pongRef ! Ping(pingRef, x + 1)
              Next()
            else
              pongRef ! Done(x)
              pingRef ! Done(x)
              Next()
          case Done(x) =>
            Stop(x)
      }(algorithm)
    }

  val pongActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (y: PingPong, pongRef: Ponger) =>
        y match
          case Ping(pingRef, x) =>
            if x < maxHits then
              pingRef ! Pong(pongRef, x + 1)
              Next()
            else
              pingRef ! Done(x)
              pongRef ! Done(x)
              Next()
          case Done(x) =>
            Stop(x)
      }(algorithm)
    }

  (pingActor, pongActor)
