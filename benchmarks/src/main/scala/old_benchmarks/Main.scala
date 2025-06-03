package old_benchmarks

import join_actors.api.MatchingAlgorithm
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
      rndMsgsStep: Int = 4,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSmartHouseBenchmark(
      smartHouseActions = matches,
      maxRandomMsgs = randomMsgs,
      rndMsgsStep = rndMsgsStep,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

  @main
  def simpleSmartHouseConfig(
      config: Config,
      @arg(short = 'm', doc = "The maximum number of matches the smart house actor should perform")
      matches: Int = 100,
      @arg(
        short = 'r',
        doc = "The maximum number of prefix messages the smart house actor should process"
      )
      randomMsgs: Int = 20,
      @arg(short = 's', doc = "The step by which the number of prefix messages should increase")
      rndMsgsStep: Int = 4,
      @arg(short = 'g', doc = "Whether to use heavy guards")
      withHeavyGuard: Boolean = false,
      @arg(short = 'f', doc = "The file to write the messages (as JSON) used to run the benchmarks")
      jsonFileToWrite: String,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val jsonFilePath      = os.RelPath(jsonFileToWrite).resolveFrom(os.pwd)
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSimpleSmartHouseBenchmark(
      smartHouseActions = matches,
      maxRandomMsgs = randomMsgs,
      rndMsgsStep = rndMsgsStep,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      withHeavyGuard = withHeavyGuard,
      pathToJsonDataFile = jsonFilePath,
      pathForBenchmarkData = benchmarkDataPath
    )

  @main
  def boundedBufferConfig(
      config: Config,
      @arg(short = 'b', doc = "The buffer bound")
      bufferBound: Int = 100,
      @arg(
        short = 'c',
        doc = "The maximum number of producers and consumers"
      )
      nProdsCons: Int = 50,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runBBBenchmark(
      bufferBound = bufferBound,
      nProdsCons = nProdsCons,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

  @main
  def sizeConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSizeBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

  @main
  def sizeWithGuardsConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSizeWithGuardsBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

  @main
  def sizeWithNoiseConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSizeWithNoiseBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

  @main
  def sizeWithGuardsWithNoiseConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSizeWithGuardsWithNoiseBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

  @main
  def sizeWithGuardsWithNonMatchingPayloadsConfig(
      config: Config,
      @arg(short = 'm', doc = "The number of matches the size actor should perform")
      matches: Int = 100,
      @arg(short = 'p', doc = "The file path to write the benchmark data")
      dataOutputFilePath: String
  ) =
    val benchmarkDataPath = os.RelPath(dataOutputFilePath).resolveFrom(os.pwd)
    runSizeWithGuardsWithNonMatchingPayloadBenchmark(
      matches = matches,
      writeToFile = config.writeToFile.value,
      warmupRepetitions = config.warmupRepetitions,
      repetitions = config.repetitions,
      outputDataDir = benchmarkDataPath
    )

//  def main(args: Array[String]): Unit =
//    val config = ParserForMethods(this).runOrExit(args)
//    println(config)
