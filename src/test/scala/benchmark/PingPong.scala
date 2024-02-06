package test.benchmark.pingPong

import test.ALGORITHM
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass
import test.classes.Msg
import test.classes.pingPong.*

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

def runBenchmark(maxHits: Int) =
  implicit val ec = ExecutionContext.global

  val (pingActor, pongActor) = pingPonger(maxHits)

  Future {
    val startTime = System.nanoTime()

    val (result1, pinger) = pingActor.start()
    val (result2, ponger) = pongActor.start()

    val results = Future.sequence(Seq(result1, result2))

    ponger ! Ping(pinger, 0)

    val finalResult = Await.ready(results, Duration(30, TimeUnit.SECONDS))

    if results.isCompleted then
      val endTime = System.nanoTime()
      endTime - startTime
    else -1L
  }

@main
def pingPongBenchmark =
  val maxHits = 100_000
  Benchmark(
    "Ping Pong",
    10,
    200,
    BenchmarkPass(
      "Control",
      () => runBenchmark(maxHits)
    ),
    List(
      BenchmarkPass(
        s"Macro using ${ALGORITHM.toString()}",
        () => runBenchmark(maxHits)
      )
    )
  ).run
