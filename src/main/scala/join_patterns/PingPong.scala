package join_patterns

import actor.*
import join_patterns.*
import org.scalacheck.*
import org.scalatest.run

import java.util.concurrent.TimeUnit
import scala.compiletime.ops.int
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.*

import concurrent.ExecutionContext.Implicits.global
// (
//     (_: Map[String, Any], pingRef: ActorRef[PingPong]) =>
//       if _.apply("x").asInstanceOf[Int].<(maxHits) then
//         println(
//           _root_.scala.StringContext
//             .apply("Ponged by ", " --- ping count: ", "")
//             .s(
//               _.apply("pongRef").asInstanceOf[ActorRef[Ping | Done]],
//               _.apply("x").asInstanceOf[Int]
//             )
//         )
//         _.apply("pongRef")
//           .asInstanceOf[ActorRef[Ping | Done]]
//           .!(Ping.apply(pingRef, _.apply("x").asInstanceOf[Int].+(1)))
//         Next.apply[Int]()
//       else {
//         _.apply("pongRef")
//           .asInstanceOf[ActorRef[Ping | Done]]
//           .!(Done.apply(_.apply("x").asInstanceOf[Int]))
//         pingRef.!(Done.apply(_.apply("x").asInstanceOf[Int]))
//         Next.apply[Int]()
//       }
// )
object PingPong extends App:
  type Ponger = ActorRef[Ping | Done]
  type Pinger = ActorRef[Pong | Done]

  sealed trait PingPong
  case class Ping(ref: Pinger, hits: Int) extends PingPong
  case class Pong(ref: Ponger, hits: Int) extends PingPong
  case class Done(hits: Int)              extends PingPong

  val ALGORITHM = MatchingAlgorithm.BasicAlgorithm

  def pingPonger() =
    val maxHits = 100

    val pingActor: Actor[PingPong, Int] =
      Actor[PingPong, Int](receive { (y: PingPong, pingRef: Pinger) =>
        y match
          case Pong(pongRef, x) =>
            if x < maxHits then
              // println(s"Ponged by $pongRef --- ping count: $x")
              pongRef ! Ping(pingRef, x + 1)
              Next()
            else
              pongRef ! Done(x)
              pingRef ! Done(x)
              Next()
          case Done(x) =>
            // println(s"Final count: $x")
            Stop(x)
      }(ALGORITHM))

    val pongActor: Actor[PingPong, Int] =
      Actor[PingPong, Int](receive { (y: PingPong, pongRef: Ponger) =>
        y match
          case Ping(pingRef, x) =>
            if x < maxHits then
              // println(s"Pinged by $pingRef --- pong count: $x")
              // println(s"maxHits: $maxHits")
              pingRef ! Pong(pongRef, x + 1)
              Next()
            else
              pingRef ! Done(x)
              pongRef ! Done(x)
              Next()
          case Done(x) =>
            // println(s"Final count: $x")
            Stop(x)
      }(ALGORITHM))

    val (futResult1, pinger) = pingActor.start()
    val (futResult2, ponger) = pongActor.start()

    val results = Future.sequence(Seq(futResult1, futResult2))

    pinger ! Pong(ponger, 0)

    val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

    finalResult.onComplete(printResult)

    // println("\n======================================================\n\n")

  // pingPonger()
  // Measure the time it takes to the pingPonger to finish
  // val start = System.nanoTime
  pingPonger()
  // val end = System.nanoTime
  // println(s"Time taken: ${(end - start) / 1000000} ms")
