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
  case class Config(
      @arg(doc = "repetitions")
      repetitions: Int = 2,
      @arg(doc = "warmup repetitions")
      warmup: Int = 2,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  )

  implicit def configParser: ParserForClass[Config] = ParserForClass[Config]

  def runAndOutput(
    benchmarkFactory: BenchmarkFactory,
    config: benchmarkFactory.Config,
    paramRange: Range,
    benchmarkName: String,
    paramName: String,
    outputDir: Path
    ): Unit =
      val algorithms: List[MatchingAlgorithm] =
          List(
            BruteForceAlgorithm,
//            StatefulTreeBasedAlgorithm,
//            MutableStatefulAlgorithm,
//            LazyMutableAlgorithm,
//            WhileEagerAlgorithm,
//      //      EagerParallelAlgorithm(2),
//      //      EagerParallelAlgorithm(4),
//      //      EagerParallelAlgorithm(6),
//            EagerParallelAlgorithm(8),
//            WhileLazyAlgorithm,
//      //      LazyParallelAlgorithm(2),
//      //      LazyParallelAlgorithm(4),
//      //      LazyParallelAlgorithm(6),
            LazyParallelAlgorithm(8)
          )


      val results = runBenchmarkSeries(benchmarkFactory, config, algorithms, paramRange, paramName)

      writeResults(benchmarkName, paramName, paramRange, results, outputDir)



  @main
  def simpleSmartHouse(
    runConfig: Config,
    @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
    matches: Int = 100,
    @arg(
      short = 'f',
      doc = "The minimum number of prefix messages the smart house actor should process"
    )
    minPrefixMsgs: Int = 0,
    @arg(
      short = 'r',
      doc = "The maximum number of prefix messages the smart house actor should process"
    )
    maxPrefixMsgs: Int = 20,
    @arg(short = 's', doc = "The step by which the number of prefix messages should increase")
    prefixMsgsStep: Int = 4,
    @arg(short = 'g', doc = "Whether to use heavy guards")
    withHeavyGuard: Boolean = false,
  ): Unit =
    val config = SimpleSmartHouseConfig(withHeavyGuard, matches)
    val benchmarkDataPath = os.RelPath(runConfig.dataOutputFilePath).resolveFrom(os.pwd)

    runAndOutput(
      SimpleSmartHouse,
      config,
      minPrefixMsgs to maxPrefixMsgs by prefixMsgsStep,
      "Simple Smart House",
      "Number of prefix messages per match",
      benchmarkDataPath
    )

  @main
  def smartHouseConfig(
                        config: Config,
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
