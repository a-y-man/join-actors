package benchmarks

import join_patterns.Matcher
import join_patterns.MatchingAlgorithm
import join_patterns.MatchingTree

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(Executors.newVirtualThreadPerTaskExecutor())

case class Measurement(time: FiniteDuration, matches: Int)
object Measurement:
  def apply(time: Long, matches: Int): Measurement =
    Measurement(FiniteDuration(time, TimeUnit.MILLISECONDS), matches)

  def time(measurement: Measurement): FiniteDuration = measurement.time

  def matches(measurement: Measurement): Int = measurement.matches

class BenchmarkPass(
    val name: String,
    private val mainFn: () => Future[Measurement]
):
  def warmup(warmupIterations: Int): Unit =
    Await.ready(
      Future.sequence((1 to warmupIterations).map(_ => mainFn())),
      Duration(20, TimeUnit.MINUTES)
    )

  def benchmark(iterations: Int): Seq[Measurement] =
    Await
      .result(
        Future.sequence((1 to iterations).map(_ => mainFn())),
        Duration(90, TimeUnit.MINUTES)
      )

  def run(warmupIterations: Int, iterations: Int): Seq[Measurement] =
    println(f"-- Pass $name")

    println(Console.YELLOW + "\tstart warmup" + Console.RESET)
    warmup(warmupIterations)
    println(Console.YELLOW + "\tend warmup" + Console.RESET)

    println(Console.GREEN + "\tstart benchmark" + Console.RESET)
    val elapsed = benchmark(iterations)
    println(Console.GREEN + "\tend benchmark" + Console.RESET)

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
  def displayResults(results: List[(String, Seq[Measurement])]) =
    import Console.{GREEN, RED, RESET}

    val (nullName, nullPassMeasurements) = results.head

    val nullPassElapsed = nullPassMeasurements.map(Measurement.time).reduce(_ + _)
    val nullPassMatches = nullPassMeasurements.map(Measurement.matches).sum
    val nullPassAverage = nullPassElapsed / warmupIterations

    println(
      Console.YELLOW + f"Null Pass $nullName" + Console.RESET +
        "\n\t" + f"total matches : $nullPassMatches" +
        "\n\t" + f"elapsed time : ${Console.GREEN}${nullPassElapsed}" + Console.RESET +
        "\n\t" + f"average time per iteration : ${Console.GREEN}$nullPassAverage" + Console.RESET + '\n'
    )

    val passes = results.tail

    for (passName, passRuntimes) <- passes do
      val totalMatches     = passRuntimes.map(Measurement.matches).sum
      val totalElapsedPass = passRuntimes.map(Measurement.time).reduce(_ + _)
      val passAverage      = totalElapsedPass / iterations
      val delta            = ((passAverage - nullPassAverage) * 100.0) / nullPassAverage
      val delta_formatted =
        (if delta < 0 then s"${GREEN}" else s"${RED}") + "%.2f".format(delta) + s"${RESET}"

      println(
        f"Pass $passName" +
          "\n\t" + f"total matches : $totalMatches" +
          "\n\t" + f"elapsed time : ${totalElapsedPass}" +
          "\n\t" + f"average time per iteration : $passAverage" +
          "\n\t" + f"pass speed related to null pass: $delta_formatted " + '%'
      )

  def toFile(results: List[(String, Seq[Measurement])]) =
    import java.util.Date
    import java.io.{File, PrintWriter}
    import java.text.SimpleDateFormat

    val folder    = "/home/ayhu/Documents/JoinPatterns/experiment_results/data"
    val sep       = ";"
    val timestamp = SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Date())
    val file      = PrintWriter(File(f"$folder/${timestamp}_${name}_${algorithm}.csv"))

    file.write(
      results
        .map((name, measurements) =>
          val times = measurements.map(Measurement.time).map(_.toMillis)
          s"${name.toInt} $sep ${times.mkString(sep)}"
        )
        .mkString("\n")
    )
    file.close

  def run(writeToFile: Boolean): Unit =
    println(
      f"Benchmark $name BEGIN (iterations: $iterations, warmup iterations: $warmupIterations)"
    )

    val results: List[(String, Seq[Measurement])] =
      List((nullPass.name, nullPass.run(warmupIterations, iterations))).concat(
        passes.map(p => (p.name, p.run(warmupIterations, iterations)))
      )

    println(f"Benchmark $name END")

    displayResults(results)

    if writeToFile then toFile(results.tail)
