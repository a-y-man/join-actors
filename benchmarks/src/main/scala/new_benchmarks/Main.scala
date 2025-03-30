package new_benchmarks

import join_actors.api.*
import mainargs.*
import new_benchmarks.bounded_buffer.{BoundedBuffer, BoundedBufferConfig}
import new_benchmarks.complex_smart_house.{ComplexSmartHouse, ComplexSmartHouseConfig}
import new_benchmarks.simple_smart_house.SimpleSmartHouse

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import new_benchmarks.simple_smart_house.SimpleSmartHouseConfig
import new_benchmarks.size.{Size, SizeConfig}
import new_benchmarks.size_with_guards.{GuardedSize, GuardedSizeConfig, GuardedSizeVariant}
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
    commonConfig: CommonRunConfig,
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
//        EagerParallelAlgorithm(8),
        WhileLazyAlgorithm,
//        LazyParallelAlgorithm(2),
//        LazyParallelAlgorithm(4),
//        LazyParallelAlgorithm(6),
        LazyParallelAlgorithm(8)
      )
    val paramRange = commonConfig.minParam to commonConfig.maxParam by commonConfig.paramStep

    val results = runBenchmarkSeries(
      benchmarkFactory,
      config,
      algorithms,
      paramRange,
      commonConfig.repetitions,
      commonConfig.warmup,
      paramName
    )

    val processedResults = processBenchmarkSeriesResults(results)

    val outputPathResolved = os.RelPath(commonConfig.outputPath).resolveFrom(os.pwd)
    saveResults(benchmarkName, paramName, paramRange, processedResults, outputPathResolved)

  @main
  def simpleSmartHouse(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 100,
    @arg(short = 'g', doc = "Whether to use a heavy guard")
    heavyGuard: Flag,
  ): Unit =
    val config = SimpleSmartHouseConfig(heavyGuard.value, matches)

    runAndOutput(
      commonConfig,
      SimpleSmartHouse,
      config,
      "Simple Smart House" + (if heavyGuard.value then " with a heavy guard" else ""),
      "Number of prefix messages per match"
    )

  @main
  def boundedBuffer(
    commonConfig: CommonRunConfig,
    @arg(short = 'b', doc = "The buffer bound")
    bufferBound: Int = 100,
    @arg(doc = "The number of puts/gets performed by each producer and consumer")
    count: Int = 100
  ): Unit =
    val config = BoundedBufferConfig(bufferBound, count)

    runAndOutput(
      commonConfig,
      BoundedBuffer,
      config,
      "Bounded Buffer",
      "Number of producers and consumers"
    )

  @main
  def complexSmartHouse(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 100,
  ): Unit =
    val config = ComplexSmartHouseConfig(matches)

    runAndOutput(
      commonConfig,
      ComplexSmartHouse,
      config,
      "Complex Smart House",
      "Number of random messages per match"
    )

  @main
  def size(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The number of matches the size actor should perform")
    matches: Int = 100,
    @arg(short = 'n', doc = "Whether to include noise in the messages")
    noise: Flag
  ): Unit =
    val min =
      if commonConfig.minParam >= 1 then commonConfig.minParam
      else
        println("The size benchmark does not accept a minimum parameter less than 1, setting to 1")
        1

    val max =
      if commonConfig.maxParam <= 6 then commonConfig.minParam
      else
        println("The size benchmark does not accept a maximum parameter more than 6, setting to 6")
        6

    val newCommonConfig = commonConfig.copy(minParam = min, maxParam = max)

    val config = SizeConfig(matches, noise.value)

    runAndOutput(
      newCommonConfig,
      Size,
      config,
      "Size" + (if noise.value then " with noise" else ""),
      "Arity of join pattern"
    )

  @main
  def sizeWithGuards(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The number of matches the size actor should perform")
    matches: Int = 100,
    @arg(short = 'v', doc = "The benchmark variant to run: either \"normal\", \"noisy\", or \"non-matching\"")
    variant: String = "normal"
  ): Unit =
    val min =
      if commonConfig.minParam >= 1 then commonConfig.minParam
      else
        println("The size benchmark does not accept a minimum parameter less than 1, setting to 1")
        1

    val max =
      if commonConfig.maxParam <= 6 then commonConfig.minParam
      else
        println("The size benchmark does not accept a maximum parameter more than 6, setting to 6")
        6

    val newCommonConfig = commonConfig.copy(minParam = min, maxParam = max)

    val variantEnum = variant match
      case "normal" => GuardedSizeVariant.Normal
      case "noisy" => GuardedSizeVariant.Noisy
      case "non-matching" => GuardedSizeVariant.NonMatchingPayloads
      case _ => throw MatchError(s"$variant is not a valid benchmark variant: should be either \"normal\", \"noisy\", or \"non-matching\"")

    val config = GuardedSizeConfig(matches, variantEnum)

    val descriptor = variantEnum match
      case GuardedSizeVariant.Normal => ""
      case GuardedSizeVariant.Noisy => " with noise"
      case GuardedSizeVariant.NonMatchingPayloads => " with non-matching payloads"

    runAndOutput(
      newCommonConfig,
      GuardedSize,
      config,
      "Size with guards" + descriptor,
      "Arity of join pattern"
    )

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)
