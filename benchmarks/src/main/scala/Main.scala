package benchmarks

import benchmarks.RunSimpleSmartHouseBenchmark.pathForJsonData
import join_patterns.MatchingAlgorithm
import mainargs.Flag
import mainargs.ParserForClass
import mainargs.ParserForMethods
import mainargs.arg
import mainargs.main

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main:
  @main
  case class Config(
      @arg(short = 'n', doc = "benchmark name")
      benchmarkName: String,
      @arg(doc = "warmup repetitions")
      warmupRepetitions: Int = 2,
      @arg(doc = "repetitions")
      repetitions: Int = 2,
      @arg(doc = "write to file")
      writeToFile: Flag
  )

  implicit def configParser: ParserForClass[Config] = ParserForClass[Config]

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
      rndMsgsStep: Int = 4
  ) =
    runSmartHouseBenchmark(
      smartHouseActions = matches,
      maxRandomMsgs = randomMsgs,
      rndMsgsStep = rndMsgsStep,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  // @main
  // def simpleSmartHouseConfig(
  //     config: Config,
  //     @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
  //     matches: Int = 100,
  //     @arg(
  //       short = 'r',
  //       doc = "The maximum number of prefix messages the smart house actor should process"
  //     )
  //     randomMsgs: Int = 20,
  //     @arg(short = 's', doc = "The step by which the number of prefix messages should increase")
  //     rndMsgsStep: Int = 4,
  //     @arg(short = 'f', doc = "The file to write the messages used to run the benchmarks")
  //     jsonDataToWrite: String
  // ) =
  //   val path = os.Path(jsonDataToWrite)
  //   runSimpleSmartHouseBenchmark(
  //     smartHouseActions = matches,
  //     maxRandomMsgs = randomMsgs,
  //     rndMsgsStep = rndMsgsStep,
  //     jsonFileToWrite = path,

  //     writeToFile = config.writeToFile.value,
  //     warmupRepetitions = config.warmupRepetitions,
  //     repetitions = config.repetitions
  //   )

  @main
  def boundedBufferConfig(
      config: Config,
      @arg(short = 'b', doc = "The buffer bound")
      bufferBound: Int = 100,
      @arg(
        short = 'p',
        doc = "The maximum number of producers and consumers"
      )
      nProdsCons: Int = 50
  ) =
    runBBBenchmark(
      bufferBound = bufferBound,
      nProdsCons = nProdsCons,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  @main
  def sizeConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100
  ) =
    runSizeBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  @main
  def sizeWithGuardsConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100
  ) =
    runSizeWithGuardsBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  @main
  def sizeWithNoiseConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100
  ) =
    runSizeWithNoiseBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  @main
  def sizeWithGuardsWithNoiseConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100
  ) =
    runSizeWithGuardsWithNoiseBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  @main
  def sizeWithGuardsWithNonMatchingPayloadsConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100
  ) =
    runSizeWithGuardsWithNonMatchingPayloadBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions
    )

  def main(args: Array[String]): Unit =
    val config = ParserForMethods(this).runOrExit(args)
    println(config)
