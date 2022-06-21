package test.benchmark

import scala.concurrent.duration.DurationLong
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import join_patterns.Matcher

trait Benchmarkable[M, T] extends Runnable {
  def run_as_future: Future[Long]
  def run_without_macro: Future[Long]
}

class BenchmarkPass(
    val name: String,
    private val mainFn: () => Future[Long]
) {
  def warmup(warmupIterations: Int): Unit =
    Await.ready(
      Future.sequence((0 to warmupIterations).map(_ => mainFn())),
      Duration.Inf
    )

  def benchmark(iterations: Int): Long =
    Await
      .result(
        Future.sequence((0 to iterations).map(_ => mainFn())),
        Duration.Inf
      )
      .sum

  def run(warmupIterations: Int, iterations: Int): Long =
    println(f"-- Pass $name")

    println("\tstart warmup")
    warmup(warmupIterations)
    println("\tend warmup")

    println("\tstart benchmark")
    val elapsed = benchmark(iterations)
    println("\tend benchmark")

    println(f"-- Pass $name end")

    elapsed
}

class Benchmark(
    private val name: String,
    val warmupIterations: Int,
    val iterations: Int,
    private val nullPass: BenchmarkPass,
    private val passes: Seq[BenchmarkPass]
) {
  def run: Long =
    import Console.{GREEN, RED, RESET}

    println(
      f"Benchmark $name BEGIN (iterations: $iterations, warmup iterations: $warmupIterations)"
    )

    val results: Map[String, Long] =
      Map((nullPass.name, nullPass.run(warmupIterations, iterations))).concat(
        passes
          .map(p =>
            val res = (p.name, p.run(warmupIterations, iterations))
            println
            res
          )
      )

    println(f"Benchmark $name END")

    val total               = results.values.sum
    val average             = total / results.size
    val (nullName, nullRes) = results.head

    println(
      f"Benchmark $name RESULTS" +
        "\n" + f"total elapsed time: ${total.nano.toSeconds}s" +
        "\n" + f"average time per pass: ${average.nano.toSeconds} s" + '\n'
    )

    val averagePerIt  = "%.2f".format(nullRes.toDouble / iterations)
    val averagePerItS = "%.2f".format(nullRes.nano.toSeconds.toDouble / iterations)
    println(
      f"Pass $nullName" +
        "\n\t" + f"elapsed time : ${nullRes.nano.toSeconds} s" +
        "\n\t" + f"average time per iteration : $averagePerIt ns / $averagePerItS s" + '\n'
    )

    for (passName, passResult) <- results.tail do
      val averagePerIt  = "%.2f".format(passResult.toDouble / iterations)
      val averagePerItS = "%.2f".format(passResult.nano.toSeconds.toDouble / iterations)
      val delta         = ((passResult - nullRes) * 100.0) / nullRes.toDouble
      val delta_formatted =
        (if delta < 0 then s"${GREEN}" else s"${RED}") + "%.2f".format(delta) + s"${RESET}"

      println(
        f"Pass $passName" +
          "\n\t" + f"elapsed time : ${passResult.nano.toSeconds} s" +
          "\n\t" + f"average time per iteration : $averagePerIt ns / $averagePerItS s" +
          "\n\t" + f"pass speed related to null pass: $delta_formatted " + '%'
      )

    total
}
