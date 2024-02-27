package test.benchmark

import actor.OldActor
import join_patterns.Matcher
import join_patterns.MatchingAlgorithm
import join_patterns.MatchingTree

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.Executors

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(Executors.newVirtualThreadPerTaskExecutor())

trait Benchmarkable[M, T] extends OldActor[M, T]:
  def run_as_future: Future[Long]

class BenchmarkPass(
    val name: String,
    private val mainFn: () => Future[Long]
):
  def warmup(warmupIterations: Int): Unit =
    Await.ready(
      Future.sequence((1 to warmupIterations).map(_ => mainFn())),
      Duration(20, TimeUnit.MINUTES)
    )

  def benchmark(iterations: Int): Seq[Long] =
    Await
      .result(
        Future.sequence((1 to iterations).map(_ => mainFn())),
        Duration(90, TimeUnit.MINUTES)
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

class Benchmark(
    private val name: String,
    private val algorithm: MatchingAlgorithm,
    val warmupIterations: Int,
    val iterations: Int,
    private val nullPass: BenchmarkPass,
    private val passes: Seq[BenchmarkPass]
):
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

  def toFile(results: List[(String, Seq[Long])]) =
    import java.util.Date
    import java.io.{File, PrintWriter}
    import java.text.SimpleDateFormat

    val folder    = "/home/ayhu/Documents/JoinPatterns/experiment_results/data"
    val sep       = ';'
    val timestamp = SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Date())
    val file      = PrintWriter(File(f"$folder/${timestamp}_${name}_${algorithm}.csv"))

    file.write(
      results.map((name, times) => name + sep + times.mkString(sep.toString)).mkString("\n")
    )
    file.close

  def run(writeToFile: Boolean): Long =
    println(
      f"Benchmark $name BEGIN (iterations: $iterations, warmup iterations: $warmupIterations)"
    )

    val results: List[(String, Seq[Long])] =
      List((nullPass.name, nullPass.run(warmupIterations, iterations))).concat(
        passes.map(p => (p.name, p.run(warmupIterations, iterations)))
      )

    println(f"Benchmark $name END")

    displayResults(results)

    if writeToFile then toFile(results.tail)

    results.map(_._2.sum).sum
