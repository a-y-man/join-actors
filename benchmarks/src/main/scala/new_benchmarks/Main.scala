package new_benchmarks

import join_actors.api.*
import mainargs.*
import new_benchmarks.bounded_buffer.BoundedBuffer
import new_benchmarks.bounded_buffer.BoundedBufferConfig
import new_benchmarks.complex_smart_house.ComplexSmartHouse
import new_benchmarks.complex_smart_house.ComplexSmartHouseConfig
import new_benchmarks.simple_smart_house.SimpleSmartHouse
import new_benchmarks.simple_smart_house.SimpleSmartHouseConfig
import new_benchmarks.size.Size
import new_benchmarks.size.SizeConfig
import new_benchmarks.size_with_guards.GuardedSize
import new_benchmarks.size_with_guards.GuardedSizeConfig
import new_benchmarks.size_with_guards.GuardedSizeVariant
import os.*
import os.Path

import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.Queue
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

object Main:
  @main
  case class CommonRunConfig(
      @arg(doc =
        "The matcher to use are separated by commas and enclosed in quotes, or \"all\" for all matcher, default all. " +
          "Matcher options: " + MatcherSelector.CMD_STRINGS.mkString(", ")
      )
      matchers: String = "all",
      @arg(doc = "Matchers to exclude, separated by commas and enclosed in quotes")
      exclude: String = "",
      @arg(doc = "The minimum parameter value, default 0")
      minParam: Int = 0,
      @arg(doc = "The step by which the parameter value should increase, default 1")
      paramStep: Int = 1,
      @arg(doc = "The maximum parameter value, default 20")
      maxParam: Int = 20,
      @arg(doc = "The number of repetitions for each parameter value, default 1")
      repetitions: Int = 1,
      @arg(doc =
        "The number of parameter values to copy as warmup repetitions. If not set, it will be the number of parameter values divided by 4"
      )
      warmup: Option[Int] = None,
      @arg(
        short = 'p',
        doc = "The folder path to which to write the benchmark results, default \"data\""
      )
      path: String = "benchmarks/data",
      @arg(doc = "Prevent generation of a plot of the results")
      suppressPlot: Flag,
      @arg(doc = "Transpose param values vs repetitions for smoother results")
      smoothen: Flag
  )

  implicit def configParser: ParserForClass[CommonRunConfig] = ParserForClass[CommonRunConfig]

  private def parseMatchersFromString(matchers: String): Seq[MatcherFactory] =
    matchers
      .split(",")
      .iterator
      .map(_.trim)
      .foldLeft(Queue()): (acc, str) =>
        MatcherSelector.parseFromCmdString(str) match
          case Some(matcher) => acc :+ matcher
          case None =>
            throw IllegalArgumentException(
              s"$str is not a valid matcher, should be one of the following: "
                + System.lineSeparator() + MatcherSelector.CMD_STRINGS.mkString(", ")
            )

  private def runAndOutput(
      commonConfig: CommonRunConfig,
      benchmarkFactory: BenchmarkFactory,
      config: benchmarkFactory.Config,
      benchmarkName: String,
      paramName: String
  ): Unit =
    val matchers =
      val matchersPreExclusion =
        if commonConfig.matchers == "all" then MatcherSelector.MATCHERS
        else parseMatchersFromString(commonConfig.matchers)

      if commonConfig.exclude == "" then matchersPreExclusion
      else
        val toExclude = parseMatchersFromString(commonConfig.exclude)
        matchersPreExclusion.filterNot(toExclude.contains(_))

    println(
      s"Running benchmark $benchmarkName with the following matchers: " + matchers.mkString(", ")
    )

    if commonConfig.maxParam < commonConfig.minParam then
      throw IllegalArgumentException(
        "Maximum parameter value cannot be less than minimum parameter value"
      )
    if commonConfig.minParam < 0 then
      throw IllegalArgumentException(
        "Minimum parameter value cannot be less than 0"
      )

    val paramRange = commonConfig.minParam to commonConfig.maxParam by commonConfig.paramStep

    val warmup = commonConfig.warmup match
      case Some(v) => v
      case None => paramRange.size / 4

    val results = runBenchmarkSeries(
      benchmarkFactory,
      config,
      matchers,
      paramRange,
      commonConfig.repetitions,
      warmup,
      paramName,
      commonConfig.smoothen.value
    )

    val processedResults = processBenchmarkSeriesResults(results)

    val outputPathResolved = os.RelPath(commonConfig.path).resolveFrom(os.pwd)
    saveResults(
      benchmarkName,
      paramName,
      paramRange,
      processedResults,
      outputPathResolved,
      !commonConfig.suppressPlot.value
    )

  private def describeMatches(matches: Int) = s" with $matches matches"

  @main
  def simpleSmartHouse(
      commonConfig: CommonRunConfig,
      @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
      matches: Int = 25,
      @arg(short = 'g', doc = "Whether to use a heavy guard")
      heavyGuard: Flag
  ): Unit =
    val config = SimpleSmartHouseConfig(heavyGuard.value, matches)

    runAndOutput(
      commonConfig,
      SimpleSmartHouse,
      config,
      "Simple Smart House" + (if heavyGuard.value then " with a heavy guard"
                              else "") + describeMatches(matches),
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

    val min =
      if commonConfig.minParam >= 1 then commonConfig.minParam
      else
        println("The bounded buffer benchmark requires at least 1 producer/consumer, setting to 1")
        1

    val newCommonConfig = commonConfig.copy(minParam = min, maxParam = count)

    runAndOutput(
      newCommonConfig,
      BoundedBuffer,
      config,
      s"Bounded Buffer with Buffer Size $bufferBound",
      "Number of producers and consumers"
    )

  @main
  def complexSmartHouse(
      commonConfig: CommonRunConfig,
      @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
      matches: Int = 25
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
      @arg(short = 'v', doc = "Whether to include noise in the messages")
      noise: Flag,
      @arg(short = 'N', doc = "The number of noise messages to include")
      numberOfNoiseMsgs: Int = 100
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

    val config = SizeConfig(matches, noise.value, numberOfNoiseMsgs)

    runAndOutput(
      newCommonConfig,
      Size,
      config,
      "Performance of join pattern" + (if noise.value then " with noise" else "") + describeMatches(
        matches
      ),
      "Size of join pattern"
    )

  @main
  def sizeWithGuards(
      commonConfig: CommonRunConfig,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 3,
      @arg(
        short = 'v',
        doc = "The benchmark variant to run: either \"normal\", \"noisy\", or \"non-matching\""
      )
      variant: String = "normal",
      @arg(short = 'N', doc = "The number of noise messages to include (only for noisy variant)")
      numberOfNoiseMsgs: Option[Int] = None,
      @arg(short = 'P', doc = "The non-matching payload to include (only for non-matching variant)")
      nonMatchingPayload: Option[Int] = None
  ): Unit =
    val min =
      if commonConfig.minParam >= 1 then commonConfig.minParam
      else
        println(
          "The size with guards benchmark does not accept a minimum parameter less than 1, setting to 1"
        )
        1

    val max =
      if commonConfig.maxParam <= 6 then commonConfig.maxParam
      else
        println(
          "The size with guards benchmark does not accept a maximum parameter more than 6, setting to 6"
        )
        6

    val newCommonConfig = commonConfig.copy(minParam = min, maxParam = max)

    val variantEnum = variant match
      case "normal" => GuardedSizeVariant.Normal
      case "noisy" => GuardedSizeVariant.Noisy
      case "non-satisfying" => GuardedSizeVariant.NonMatchingPayloads
      case _ =>
        throw IllegalArgumentException(
          s"$variant is not a valid benchmark variant: should be either \"normal\", \"noisy\", or \"non-satisfying\""
        )

    val config = GuardedSizeConfig(matches, variantEnum, numberOfNoiseMsgs, nonMatchingPayload)

    println(s"${config}")

    val descriptor = variantEnum match
      case GuardedSizeVariant.Normal => ""
      case GuardedSizeVariant.Noisy => " and noise"
      case GuardedSizeVariant.NonMatchingPayloads => " and non-satisfying payloads"

    runAndOutput(
      newCommonConfig,
      GuardedSize,
      config,
      "Performance of guarded join pattern " + descriptor + describeMatches(matches),
      "Size of join pattern"
    )

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args)
