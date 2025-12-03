package benchmarks

import join_actors.api.*

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.Failure
import scala.util.Success

final case class RunMeasurement(duration: FiniteDuration, matches: Option[Int])

private def extractMatchesFrom(passConfig: Any): Option[Int] =
  def extractFromFuture(fut: Future[?]): Option[Int] =
    fut.value match
      case Some(Success((_: Long, matches: Int))) => Some(matches)
      case Some(Success(value)) =>
        value match
          case (_: Long, matches: Int) => Some(matches)
          case _ => None
      case Some(Failure(_)) => None
      case None =>
        try
          val result = Await.result(fut.asInstanceOf[Future[(Long, Int)]], Duration.Inf)
          Some(result._2)
        catch case _: Throwable => None

  passConfig match
    case fut: Future[?] => extractFromFuture(fut)
    case product: Product =>
      product.productIterator.collectFirst { case fut: Future[?] =>
        extractFromFuture(fut)
      }.flatten
    case _ => None

def runBenchmarkPass(benchmark: Benchmark[?], param: Int): RunMeasurement =
  val prereqs = benchmark.prepare(param)

  val startTime = System.nanoTime()

  benchmark.run(prereqs)

  val endTime = System.nanoTime()

  val duration = FiniteDuration(endTime - startTime, TimeUnit.NANOSECONDS)
  val matches = extractMatchesFrom(prereqs)

  RunMeasurement(duration, matches)

type Repetitions = Seq[RunMeasurement]
def runBenchmarkRepetitions(benchmark: Benchmark[?], param: Int, repetitions: Int): Repetitions =
  if repetitions > 1 then
    println()
    for rep <- 0 until repetitions yield
      print(s"\t\tRepetition $rep... ")
      val res = runBenchmarkPass(benchmark, param)
      println(s"result: ${res.duration.toNanos} ns")

      res
  else
    val res = runBenchmarkPass(benchmark, param)
    println(s"result: ${res.duration.toNanos} ns")
    Seq(res)

type BenchmarkResults = Seq[Repetitions]
def runBenchmark(
    benchmark: Benchmark[?],
    paramRange: Range,
    repetitions: Int,
    paramName: String
): BenchmarkResults =
  for param <- paramRange yield
    print(s"\tRunning benchmark with ${paramName.toLowerCase} = $param... ")
    runBenchmarkRepetitions(benchmark, param, repetitions)

type MatcherPass = Seq[RunMeasurement]
def runSmoothenedBenchmarkMatcherPass(
    benchmark: Benchmark[?],
    paramRange: Range,
    paramName: String
): MatcherPass =
  for param <- paramRange yield
    print(s"\t\tRunning benchmark with ${paramName.toLowerCase} = $param... ")
    val res = runBenchmarkPass(benchmark, param)
    println(s"result: ${res.duration.toNanos} ns")

    res

def runSmoothenedBenchmark(
    benchmark: Benchmark[?],
    paramRange: Range,
    repetitions: Int,
    paramName: String
): BenchmarkResults =
  if repetitions > 1 then
    val algoPasses = for rep <- 0 until repetitions yield
      println(s"\tRepetition $rep... ")
      runSmoothenedBenchmarkMatcherPass(benchmark, paramRange, paramName)

    algoPasses.transpose
  else
    val algoPass = runSmoothenedBenchmarkMatcherPass(benchmark, paramRange, paramName)
    for res <- algoPass yield Seq(res)

type BenchmarkSeriesResults = Seq[(MatcherFactory, BenchmarkResults)]
def runBenchmarkSeries(
    benchmarkFactory: BenchmarkFactory,
    config: benchmarkFactory.Config,
    matchers: Seq[MatcherFactory],
    paramRange: Range,
    repetitions: Int,
    warmupSegment: Int,
    paramName: String,
    smoothen: Boolean
): BenchmarkSeriesResults =
  val benchmarkRunner = if smoothen then runSmoothenedBenchmark else runBenchmark

  for matcher <- matchers yield
    val benchmark = benchmarkFactory(matcher, config)

    println()
    if warmupSegment > 0 then
      println(s"Running warmup for $matcher")
      benchmarkRunner(benchmark, paramRange.take(warmupSegment), repetitions, paramName)
    else println("Skipping warmup")

    println(s"Running benchmark for $matcher")
    (matcher, benchmarkRunner(benchmark, paramRange, repetitions, paramName))
