package test.classes.pingPong

import join_actors.api.*

type Ponger = ActorRef[Ping | Done]
type Pinger = ActorRef[Pong | Done]

sealed trait PingPong
case class Ping(ref: Pinger, hits: Int) extends PingPong
case class Pong(ref: Ponger, hits: Int) extends PingPong
case class Done(hits: Int) extends PingPong

def pingPonger(maxHits: Int = 100, matcher: MatcherFactory) =
  val pingActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (pingRef: Pinger) =>
        {
          case Pong(pongRef, x) =>
            if x < maxHits then
              pongRef ! Ping(pingRef, x + 1)
              Continue
            else
              pongRef ! Done(x)
              pingRef ! Done(x)
              Continue
          case Done(x) =>
            Stop(x)
        }
      }(matcher)
    }

  val pongActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (pongRef: Ponger) =>
        {
          case Ping(pingRef, x) =>
            if x < maxHits then
              pingRef ! Pong(pongRef, x + 1)
              Continue
            else
              pingRef ! Done(x)
              pongRef ! Done(x)
              Continue
          case Done(x) =>
            Stop(x)
        }
      }(matcher)
    }

  (pingActor, pongActor)
