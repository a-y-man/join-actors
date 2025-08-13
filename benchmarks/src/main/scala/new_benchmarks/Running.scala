package new_benchmarks

import join_actors.api.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

def runBenchmarkPass(benchmark: Benchmark[?], param: Int): FiniteDuration =
  val prereqs = benchmark.prepare(param)

  val startTime = System.nanoTime()

  benchmark.run(prereqs)

  val endTime = System.nanoTime()

  FiniteDuration(endTime - startTime, TimeUnit.NANOSECONDS)

type Repetitions = Seq[FiniteDuration]
def runBenchmarkRepetitions(benchmark: Benchmark[?], param: Int, repetitions: Int): Repetitions =
  if repetitions > 1 then
    println()
    for rep <- 0 until repetitions yield
      print(s"\t\tRepetition $rep... ")
      val res = runBenchmarkPass(benchmark, param)
      println(s"result: ${res.toMillis} ms")

      res
  else
    val res = runBenchmarkPass(benchmark, param)
    println(s"result: ${res.toMillis} ms")
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

type MatcherPass = Seq[FiniteDuration]
def runSmoothenedBenchmarkMatcherPass(
    benchmark: Benchmark[?],
    paramRange: Range,
    paramName: String
): MatcherPass =
  for param <- paramRange yield
    print(s"\t\tRunning benchmark with ${paramName.toLowerCase} = $param... ")
    val res = runBenchmarkPass(benchmark, param)
    println(s"result: ${res.toMillis} ms")

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
