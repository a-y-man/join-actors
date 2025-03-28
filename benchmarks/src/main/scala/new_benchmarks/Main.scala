package new_benchmarks

import join_actors.api.*
import mainargs.*
import new_benchmarks.bounded_buffer.{BoundedBuffer, BoundedBufferConfig}
import new_benchmarks.complex_smart_house.{ComplexSmartHouse, ComplexSmartHouseConfig}
import new_benchmarks.simple_smart_house.SimpleSmartHouse

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import new_benchmarks.simple_smart_house.SimpleSmartHouseConfig
import os.Path
import os.*

import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.FiniteDuration

object Main:
  @main
  case class CommonRunConfig(
    @arg(
      short = 'f',
      doc = "The minimum parameter value"
    )
    minParam: Int = 0,
    @arg(short = 's', doc = "The step by which the parameter value should increase")
    paramStep: Int = 1,
    @arg(
      short = 'r',
      doc = "The maximum parameter value"
    )
    maxParam: Int = 20,
    @arg(doc = "The number of repetitions for each parameter value")
    repetitions: Int = 1,
    @arg(doc = "The number of parameter values to copy as warmup repetitions")
    warmup: Int = 10,
    @arg(short = 'p', doc = "The folder path in which to write the benchmark data")
    outputPath: String
  )

  implicit def configParser: ParserForClass[CommonRunConfig] = ParserForClass[CommonRunConfig]

  def runAndOutput(
    runConfig: CommonRunConfig,
    benchmarkFactory: BenchmarkFactory,
    config: benchmarkFactory.Config,
    benchmarkName: String,
    paramName: String
  ): Unit =
    val algorithms: List[MatchingAlgorithm] =
      List(
//        BruteForceAlgorithm,
        StatefulTreeBasedAlgorithm,
        MutableStatefulAlgorithm,
        LazyMutableAlgorithm,
        WhileEagerAlgorithm,
//        EagerParallelAlgorithm(2),
//        EagerParallelAlgorithm(4),
//        EagerParallelAlgorithm(6),
        EagerParallelAlgorithm(8),
        WhileLazyAlgorithm,
//        LazyParallelAlgorithm(2),
//        LazyParallelAlgorithm(4),
//        LazyParallelAlgorithm(6),
        LazyParallelAlgorithm(8)
      )
    val paramRange = runConfig.minParam to runConfig.maxParam by runConfig.paramStep

    val results = runBenchmarkSeries(
      benchmarkFactory,
      config,
      algorithms,
      paramRange,
      runConfig.repetitions,
      runConfig.warmup,
      paramName
    )

    val processedResults = processBenchmarkSeriesResults(results)

    val outputPathResolved = os.RelPath(runConfig.outputPath).resolveFrom(os.pwd)
    saveResults(benchmarkName, paramName, paramRange, processedResults, outputPathResolved)

  @main
  def simpleSmartHouse(
    runConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 100,
    @arg(short = 'g', doc = "Whether to use heavy guards")
    withHeavyGuard: Boolean = false,
  ): Unit =
    val config = SimpleSmartHouseConfig(withHeavyGuard, matches)

    runAndOutput(
      runConfig,
      SimpleSmartHouse,
      config,
      "Simple Smart House",
      "Number of prefix messages per match"
    )

  @main
  def boundedBuffer(
    runConfig: CommonRunConfig,
    @arg(short = 'b', doc = "The buffer bound")
    bufferBound: Int = 100,
    @arg(doc = "The number of puts/gets performed by each producer and consumer")
    count: Int = 100
  ): Unit =
    val config = BoundedBufferConfig(bufferBound, count)

    runAndOutput(
      runConfig,
      BoundedBuffer,
      config,
      "Bounded Buffer",
      "Number of producers and consumers"
    )

  @main
  def complexSmartHouse(
    runConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 100,
  ): Unit =
    val config = ComplexSmartHouseConfig(matches)

    runAndOutput(
      runConfig,
      ComplexSmartHouse,
      config,
      "Complex Smart House",
      "Number of random messages per match"
    )


  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)
