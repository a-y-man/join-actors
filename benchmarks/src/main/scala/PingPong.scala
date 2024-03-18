package benchmarks

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import java.util.concurrent.Executors
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

def measurePingPong(maxHits: Int, algorithm: MatchingAlgorithm) =

  val (pingActor, pongActor) = pingPonger(maxHits, algorithm)
  val (result1, pinger)      = pingActor.start()
  val (result2, ponger)      = pongActor.start()

  Future {
    val startTime = System.currentTimeMillis()

    val results = Future.sequence(Seq(result1, result2))

    ponger ! Ping(pinger, 0)

    Await.ready(results, Duration(30, TimeUnit.SECONDS))

    val endTime = System.currentTimeMillis()
    Measurement(endTime - startTime, maxHits)
  }

def pingPongBenchmark(maxHits: Int, algorithm: MatchingAlgorithm) =
  Benchmark(
    "Ping Pong",
    algorithm,
    10,
    10,
    BenchmarkPass(
      "Control Null Pass",
      () => measurePingPong(maxHits, algorithm)
    ),
    List(
      BenchmarkPass(
        s"PingPong using ${algorithm}",
        () => measurePingPong(maxHits, algorithm)
      )
    )
  )

def runPingPongBenchmark(maxHits: Int, writeToFile: Boolean = false) =
  val statefulTreeAlgorithm = MatchingAlgorithm.StatefulTreeBasedAlgorithm
  val bruteForceAlgorithm   = MatchingAlgorithm.BruteForceAlgorithm

  List(bruteForceAlgorithm, statefulTreeAlgorithm) foreach { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    pingPongBenchmark(maxHits, algorithm).run(writeToFile)
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )
  }
