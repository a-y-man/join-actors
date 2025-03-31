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

type BenchmarkSeriesResults = Seq[(MatchingAlgorithm, BenchmarkResults)]
def runBenchmarkSeries(
  benchmarkFactory: BenchmarkFactory,
  config: benchmarkFactory.Config,
  algorithms: Seq[MatchingAlgorithm],
  paramRange: Range,
  repetitions: Int,
  warmupSegment: Int,
  paramName: String,
  ): BenchmarkSeriesResults =
  for algo <- algorithms yield
    val benchmark = benchmarkFactory(algo, config)

    println()
    if warmupSegment > 0 then
      println(s"Running warmup for $algo")
      runBenchmark(benchmark, paramRange.take(warmupSegment), repetitions, paramName)
    else
      println("Skipping warmup")

    println(s"Running benchmark for $algo")
    (algo, runBenchmark(benchmark, paramRange, repetitions, paramName))
