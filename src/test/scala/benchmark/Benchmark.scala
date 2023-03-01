package test.benchmark

import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import join_patterns.Matcher
import actor.Actor
import java.util.concurrent.TimeUnit

trait Benchmarkable[M, T] extends Actor[M, T] {
  def run_as_future: Future[Long]
  def run_without_macro: Future[Long]
}

class BenchmarkPass(
    val name: String,
    private val mainFn: () => Future[Long]
) {
  def warmup(warmupIterations: Int): Unit =
    Await.ready(
      Future.sequence((1 to warmupIterations).map(_ => mainFn())),
      Duration(5, TimeUnit.MINUTES)
    )

  def benchmark(iterations: Int): Seq[Long] =
    Await
      .result(
        Future.sequence((1 to iterations).map(_ => mainFn())),
        Duration(5, TimeUnit.MINUTES)
      )

  def run(warmupIterations: Int, iterations: Int): Seq[Long] =
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
  def displayResults(results: List[(String, Seq[Long])]) =
    import Console.{GREEN, RED, RESET}

    val total               = results.map(_._2.sum).sum
    val average             = total / results.size
    val (nullName, nullRes) = (results(0)._1, results(0)._2.sum)

    println(
      f"Benchmark $name RESULTS" +
        "\n" + "total elapsed time: " + "%.2f".format(total / 1e9) + "s" +
        "\n" + "average time per pass: " + "%.2f".format(average / 1e9) + "s" + '\n'
    )

    val averagePerIt  = "%.2f".format(nullRes.toDouble / iterations)
    val averagePerItS = "%.4f".format((nullRes.toDouble / iterations) / 1e9)
    println(
      f"Pass $nullName" +
        "\n\t" + f"elapsed time : " + "%.2f".format(nullRes / 1e9) + "s" +
        "\n\t" + f"average time per iteration : $averagePerIt ns / $averagePerItS s" + '\n'
    )

    for (passName, passResult) <- results.tail do
      val res           = passResult.sum
      val averagePerIt  = "%.2f".format(res.toDouble / iterations)
      val averagePerItS = "%.4f".format((res.toDouble / iterations) / 1e9)
      val delta         = ((res - nullRes) * 100.0) / nullRes.toDouble
      val delta_formatted =
        (if delta < 0 then s"${GREEN}" else s"${RED}") + "%.2f".format(delta) + s"${RESET}"

      println(
        f"Pass $passName" +
          "\n\t" + f"elapsed time : " + "%.2f".format(res / 1e9) + "s" +
          "\n\t" + f"average time per iteration : $averagePerIt ns / $averagePerItS s" +
          "\n\t" + f"pass speed related to null pass: $delta_formatted " + '%'
      )

  def boxplot(results: List[(String, Seq[Long])]): List[(String, Seq[Double])] =
    results.map((n, t) =>
      val _t = t.sorted
      val lw = _t(0) / 1e6
      val q1 = _t((_t.size * 0.25).ceil.toInt) / 1e6
      val md = _t((_t.size * 0.5).ceil.toInt) / 1e6
      val q3 = _t((_t.size * 0.75).ceil.toInt) / 1e6
      val hw = _t.last / 1e6

      (n, List(lw, q1, md, q3, hw))
    )

  def toFile(results: List[(String, Seq[Long])]) =
    import java.util.Date
    import java.io.{File, PrintWriter}
    import java.text.SimpleDateFormat

    val folder    = "data"
    val sep       = ';'
    val timestamp = SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Date())
    val file      = PrintWriter(File(f"$folder/${name}_$timestamp.csv"))

    file.write(
      results.map((name, times) => name + sep + times.mkString(sep.toString)).mkString("\n")
    )
    file.close

  def run: Long =
    println(
      f"Benchmark $name BEGIN (iterations: $iterations, warmup iterations: $warmupIterations)"
    )

    val results: List[(String, Seq[Long])] =
      List((nullPass.name, nullPass.run(warmupIterations, iterations))).concat(
        passes.map(p => (p.name, p.run(warmupIterations, iterations)))
      )

    println(f"Benchmark $name END")

    displayResults(results)
    // toFile(results)

    results.map(_._2.sum).sum
}
