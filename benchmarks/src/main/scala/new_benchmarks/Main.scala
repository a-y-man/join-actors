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
import scala.collection.immutable.{ArraySeq, Queue}
import scala.concurrent.duration.FiniteDuration

object Main:
  @main
  case class CommonRunConfig(
    @arg(doc = "The algorithms to use separated by commas and enclosed in quotes, or \"all\" for all algorithms, default all. " +
      "Algorithm options: " + MatchingAlgorithm.CMD_STRINGS.mkString(", "))
    algorithms: String = "all",
    @arg(doc = "Algorithms to exclude, separated by commas and enclosed in quotes")
    exclude: String = "",
    @arg(doc = "The minimum parameter value, default 0")
    minParam: Int = 0,
    @arg(doc = "The step by which the parameter value should increase, default 1")
    paramStep: Int = 1,
    @arg(doc = "The maximum parameter value, default 20")
    maxParam: Int = 20,
    @arg(doc = "The number of repetitions for each parameter value, default 1")
    repetitions: Int = 1,
    @arg(doc = "The number of parameter values to copy as warmup repetitions, default 10")
    warmup: Int = 10,
    @arg(short = 'p', doc = "The folder path in which to write the benchmark data")
    path: String
  )

  implicit def configParser: ParserForClass[CommonRunConfig] = ParserForClass[CommonRunConfig]

  private def parseAlgoListCmdString(algorithms: String): Seq[MatchingAlgorithm] =
    algorithms.split(",").iterator
      .map(_.trim)
      .foldLeft(Queue()): (acc, str) =>
        MatchingAlgorithm.parseFromCmdString(str) match
          case Some(algo) => acc :+ algo
          case None => throw IllegalArgumentException(
            s"$str is not a valid matching algorithm, should be one of the following: "
              + System.lineSeparator() + MatchingAlgorithm.CMD_STRINGS.mkString(", ")
          )

  private def runAndOutput(
    commonConfig: CommonRunConfig,
    benchmarkFactory: BenchmarkFactory,
    config: benchmarkFactory.Config,
    benchmarkName: String,
    paramName: String
  ): Unit =
    val algorithms =
      val algorithmsPreExclusion =
        if commonConfig.algorithms == "all" then MatchingAlgorithm.ALGORITHMS
        else parseAlgoListCmdString(commonConfig.algorithms)

      if commonConfig.exclude == "" then algorithmsPreExclusion
      else
        val toExclude = parseAlgoListCmdString(commonConfig.exclude)
        algorithmsPreExclusion.filterNot(toExclude.contains(_))

    println(s"Running benchmark $benchmarkName with the following algorithms: " + algorithms.mkString(", "))

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

    val outputPathResolved = os.RelPath(commonConfig.path).resolveFrom(os.pwd)
    saveResults(benchmarkName, paramName, paramRange, processedResults, outputPathResolved)

  private def describeMatches(matches: Int) = s" with $matches matches"

  @main
  def simpleSmartHouse(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 25,
    @arg(short = 'g', doc = "Whether to use a heavy guard")
    heavyGuard: Flag,
  ): Unit =
    val config = SimpleSmartHouseConfig(heavyGuard.value, matches)

    runAndOutput(
      commonConfig,
      SimpleSmartHouse,
      config,
      "Simple Smart House" + (if heavyGuard.value then " with a heavy guard" else "") + describeMatches(matches),
      "Number of prefix messages per match"
    )

  @main
  def boundedBuffer(
    commonConfig: CommonRunConfig,
    @arg(doc = "The buffer bound")
    bufferBound: Int = 100,
    @arg(doc = "The number of puts/gets performed by each producer and consumer")
    count: Int = 100
  ): Unit =
    val config = BoundedBufferConfig(bufferBound, count)

    runAndOutput(
      commonConfig,
      BoundedBuffer,
      config,
      s"Bounded Buffer with bufferBound $bufferBound and count $count",
      "Number of producers and consumers"
    )

  @main
  def complexSmartHouse(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 25,
  ): Unit =
    val config = ComplexSmartHouseConfig(matches)

    runAndOutput(
      commonConfig,
      ComplexSmartHouse,
      config,
      "Complex Smart House" + describeMatches(matches),
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
      if commonConfig.maxParam <= 6 then commonConfig.maxParam
      else
        println("The size benchmark does not accept a maximum parameter more than 6, setting to 6")
        6

    val newCommonConfig = commonConfig.copy(minParam = min, maxParam = max)

    val config = SizeConfig(matches, noise.value)

    runAndOutput(
      newCommonConfig,
      Size,
      config,
      "Size" + (if noise.value then " with noise" else "") + describeMatches(matches),
      "Arity of join pattern"
    )

  @main
  def sizeWithGuards(
    commonConfig: CommonRunConfig,
    @arg(short = 'm', doc = "The number of matches the size actor should perform")
    matches: Int = 3,
    @arg(short = 'v', doc = "The benchmark variant to run: either \"normal\", \"noisy\", or \"non-matching\"")
    variant: String = "normal"
  ): Unit =
    val min =
      if commonConfig.minParam >= 1 then commonConfig.minParam
      else
        println("The size with guards benchmark does not accept a minimum parameter less than 1, setting to 1")
        1

    val max =
      if commonConfig.maxParam <= 6 then commonConfig.maxParam
      else
        println("The size with guards benchmark does not accept a maximum parameter more than 6, setting to 6")
        6

    val newCommonConfig = commonConfig.copy(minParam = min, maxParam = max)

    val variantEnum = variant match
      case "normal" => GuardedSizeVariant.Normal
      case "noisy" => GuardedSizeVariant.Noisy
      case "non-satisfying" => GuardedSizeVariant.NonMatchingPayloads
      case _ => throw MatchError(s"$variant is not a valid benchmark variant: should be either \"normal\", \"noisy\", or \"non-satisfying\"")

    val config = GuardedSizeConfig(matches, variantEnum)

    val descriptor = variantEnum match
      case GuardedSizeVariant.Normal => ""
      case GuardedSizeVariant.Noisy => " and noise"
      case GuardedSizeVariant.NonMatchingPayloads => " and non-satisfying payloads"

    runAndOutput(
      newCommonConfig,
      GuardedSize,
      config,
      "Size with guards" + descriptor + describeMatches(matches),
      "Arity of join pattern"
    )

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)
