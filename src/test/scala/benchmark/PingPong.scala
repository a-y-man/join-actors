package test.benchmark.pingPong

import join_patterns.MatchingAlgorithm
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass
import test.benchmark.Measurement
import test.classes.Msg
import test.classes.pingPong.*

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

def measurePingPong(maxHits: Int, algorithm: MatchingAlgorithm) =
  implicit val ec =
    ExecutionContext.fromExecutorService(Executors.newVirtualThreadPerTaskExecutor())

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

@main
def runPingPongBenchmark() =
  val statefulTreeAlgorithm = MatchingAlgorithm.StatefulTreeBasedAlgorithm
  val bruteForceAlgorithm   = MatchingAlgorithm.BruteForceAlgorithm

  val maxHits = 10_000

  List(bruteForceAlgorithm, statefulTreeAlgorithm) foreach { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    pingPongBenchmark(maxHits, algorithm).run(false)
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )
  }
