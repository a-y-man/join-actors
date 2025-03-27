package new_benchmarks

import join_actors.api.*
import mainargs.*
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
    paramStep: Int = 4,
    @arg(
      short = 'r',
      doc = "The maximum parameter value"
    )
    maxParam: Int = 20,
    @arg(doc = "The number of repetitions for each parameter value")
    repetitions: Int = 2,
    @arg(doc = "The number of parameter values to copy as warmup repetitions")
    warmup: Int = 2,
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
  //            StatefulTreeBasedAlgorithm,
              MutableStatefulAlgorithm,
              LazyMutableAlgorithm,
  //            WhileEagerAlgorithm,
  //            EagerParallelAlgorithm(2),
  //            EagerParallelAlgorithm(4),
  //            EagerParallelAlgorithm(6),
  //            EagerParallelAlgorithm(8),
              WhileLazyAlgorithm,
  //            LazyParallelAlgorithm(2),
  //            LazyParallelAlgorithm(4),
  //            LazyParallelAlgorithm(6),
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
  def smartHouseConfig(
    config: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 100,
    @arg(
    short = 'r',
    doc = "The maximum number of random messages the smart house actor should process"
    )
    randomMsgs: Int = 32,
    @arg(short = 's', doc = "The step by which the number of random messages should increase")
    rndMsgsStep: Int = 4,
    @arg(short = 'p', doc = "The file path to write the benchmark data")
    dataOutputFilePath: String
  ): Unit =
    ???


  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)
