package new_benchmarks

import join_patterns.matching.MatchingAlgorithm
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

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
def runBenchmark(benchmark: Benchmark[?], paramRange: Range, repetitions: Int, paramName: String)
: BenchmarkResults =
  for param <- paramRange yield
    print(s"\tRunning benchmark with ${paramName.toLowerCase} = $param... ")
    runBenchmarkRepetitions(benchmark, param, repetitions)

type AlgoPass = Seq[FiniteDuration]
def runSmoothenedBenchmarkAlgoPass(benchmark: Benchmark[?], paramRange: Range, paramName: String)
: AlgoPass =
  for param <- paramRange yield
    print(s"\t\tRunning benchmark with ${paramName.toLowerCase} = $param... ")
    val res = runBenchmarkPass(benchmark, param)
    println(s"result: ${res.toMillis} ms")

    res

def runSmoothenedBenchmark(benchmark: Benchmark[?], paramRange: Range, repetitions: Int, paramName: String)
: BenchmarkResults =
  if repetitions > 1 then
    val algoPasses = for rep <- 0 until repetitions yield
      println(s"\tRepetition $rep... ")
      runSmoothenedBenchmarkAlgoPass(benchmark, paramRange, paramName)

    algoPasses.transpose
  else
    val algoPass = runSmoothenedBenchmarkAlgoPass(benchmark, paramRange, paramName)
    for res <- algoPass yield Seq(res)

type BenchmarkSeriesResults = Seq[(MatchingAlgorithm, BenchmarkResults)]
def runBenchmarkSeries(
  benchmarkFactory: BenchmarkFactory,
  config: benchmarkFactory.Config,
  algorithms: Seq[MatchingAlgorithm],
  paramRange: Range,
  repetitions: Int,
  warmupSegment: Int,
  paramName: String,
  smoothen: Boolean
): BenchmarkSeriesResults =
  val benchmarkRunner = if smoothen then runSmoothenedBenchmark else runBenchmark

  for algo <- algorithms yield
    val benchmark = benchmarkFactory(algo, config)

    println()
    if warmupSegment > 0 then
      println(s"Running warmup for $algo")
      benchmarkRunner(benchmark, paramRange.take(warmupSegment), repetitions, paramName)
    else
      println("Skipping warmup")

    println(s"Running benchmark for $algo")
    (algo, benchmarkRunner(benchmark, paramRange, repetitions, paramName))
