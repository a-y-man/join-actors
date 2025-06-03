package old_benchmarks

import join_actors.api.MatchingAlgorithm
import os.Path

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.{Duration, FiniteDuration}

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
  def warmup(warmupRepetitions: Int): Unit =
    Await.ready(
      Future.sequence((1 to warmupRepetitions).map(_ => mainFn())),
      Duration.Inf
    )

  def benchmark(repetitions: Int): Seq[Measurement] =
    (1 to repetitions).map(_ => Await.result(mainFn(), Duration.Inf))
//    Await
//      .result(
//        Future.sequence((1 to repetitions).map(_ => mainFn())),
//        Duration.Inf
//      )

  def run(warmupRepetitions: Int, iterations: Int): Seq[Measurement] =
    println(f"-- Pass $name")

    println(Console.YELLOW + "\tstart warmup" + Console.RESET)
    warmup(warmupRepetitions)
    println(Console.YELLOW + "\tend warmup" + Console.RESET)

    println(Console.GREEN + "\tstart benchmark" + Console.RESET)
    val elapsed = benchmark(iterations)
    println(Console.GREEN + "\tend benchmark" + Console.RESET)

    println(f"-- Pass $name end")

    elapsed

class Benchmark(
    private val name: String,
    private val algorithm: MatchingAlgorithm,
    val warmupRepetitions: Int,
    val repetitions: Int,
    private val nullPass: BenchmarkPass,
    private val passes: Seq[BenchmarkPass]
):
  private def displayResults(results: List[(String, Seq[Measurement])]) =
    import Console.{GREEN, RED, RESET}

    val (nullName, nullPassMeasurements) = results.head

    val nullPassElapsed = nullPassMeasurements.map(Measurement.time).reduce(_ + _)
    val nullPassMatches = nullPassMeasurements.map(Measurement.matches).sum
//    val nullPassAverage = nullPassElapsed / warmupRepetitions
    val nullPassAverage = nullPassElapsed

    println(
      Console.YELLOW + f"Null Pass $nullName" + Console.RESET +
        "\n\t" + f"total matches : $nullPassMatches" +
        "\n\t" + f"total elapsed time : ${Console.GREEN}${nullPassElapsed}" + Console.RESET +
        "\n\t" + f"average time per pass : ${Console.GREEN}${nullPassAverage}" + Console.RESET + '\n'
    )

    val passes = results.tail

    for (passName, passRuntimes) <- passes do
      val totalMatches     = passRuntimes.map(Measurement.matches).sum
      val totalElapsedPass = passRuntimes.map(Measurement.time).reduce(_ + _)
      val passAverage      = totalElapsedPass / repetitions
      val delta            = ((passAverage - nullPassAverage) * 100.0) / nullPassAverage
      val delta_formatted =
        (if delta < 0 then s"${GREEN}" else s"${RED}") + "%.2f".format(delta) + s"${RESET}"

      println(
        f"Pass $passName" +
          "\n\t" + f"total matches : $totalMatches" +
          "\n\t" + f"total elapsed time : ${totalElapsedPass}" +
          "\n\t" + f"average time per pass : ${passAverage}" +
          "\n\t" + f"pass speed related to null pass: $delta_formatted " + '%'
      )

  def run(): List[(String, Seq[Measurement])] =
    println(
      f"Benchmark $name BEGIN (iterations: $repetitions, warmup iterations: $warmupRepetitions)"
    )

    val results: List[(String, Seq[Measurement])] =
      List((nullPass.name, nullPass.run(warmupRepetitions, repetitions))).concat(
        passes.map(p => (p.name, p.run(warmupRepetitions, repetitions)))
      )

    println(f"Benchmark $name END")

    displayResults(results)

    results

def toFile(
    benchmarkName: String,
    algorithm: MatchingAlgorithm,
    results: List[(String, Seq[Measurement])],
    dataDir: Path = os.pwd / "benchmarks" / "data"
) =
  import os.*

  import java.text.SimpleDateFormat
  import java.util.Date

  val timestamp  = SimpleDateFormat("yyyy_MM_dd_HH_mm_ss").format(Date())
  val folderFile = dataDir / s"${timestamp}_${benchmarkName}"
  os.makeDir.all(folderFile)

  val file = folderFile / f"${benchmarkName}_${algorithm}.csv"

  val sep = ";"

  write(
    file,
    results
      .map((name, measurements) =>
        val times = measurements.map(Measurement.time).map(_.toMillis)
        s"${name} $sep ${times.mkString(sep)}"
      )
      .mkString("\n")
  )

def saveToFile(
    name: String,
    results: List[(MatchingAlgorithm, List[(String, Seq[Measurement])])],
    dataDir: Path = os.pwd / "benchmarks" / "data"
) =
  results foreach { (algorithm, m) => toFile(name, algorithm, m.tail, dataDir) }
