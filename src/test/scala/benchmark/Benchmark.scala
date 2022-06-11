package test.benchmark

import scala.concurrent.duration.DurationLong
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import join_patterns.Matcher

trait Benchmarkable[M, T] extends Runnable {
  // future
  def run_as_future: Future[Long]
  def run_as_future_unyielded: Future[Long]
  // base
  def run_without_macro: Future[Long]
  def run_without_macro_unyielded: Future[Long]

  protected def f: Matcher[M, T]
}

class Benchmark(
    private val name: String,
    private val mainFn: () => Future[Long],
    val warmupIterations: Int,
    val iterations: Int
) {
  def warmup: Unit =
    Await.ready(
      Future.sequence((0 to warmupIterations).map(_ => mainFn())),
      Duration.Inf
    )

  def benchmark: Long =
    Await
      .result(
        Future.sequence((0 to iterations).map(_ => mainFn())),
        Duration.Inf
      )
      .sum

  def run: Unit =
    println(f"$name benchmark iterations: $iterations, warmup iterations: $warmupIterations")

    println("start warmup")
    warmup
    println("end warmup")

    println("start benchmark")
    val elapsed = benchmark
    println("end benchmark")

    println(f"$name benchmark end")

    val inSeconds = elapsed.nano.toSeconds
    println(f"Time elapsed : $elapsed ns / $inSeconds s")
    println(
      f"Average time per iteration : ${elapsed / iterations} ns / ${inSeconds.toDouble / iterations} s"
    )
}
