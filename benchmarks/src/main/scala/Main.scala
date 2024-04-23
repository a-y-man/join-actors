package benchmarks

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
      @arg(doc = "warmup repititions")
      warmupRepititions: Int = 2,
      @arg(doc = "repititions")
      repititions: Int = 2,
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
      warmupRepititions = config.warmupRepititions,
      repititons = config.repititions
    )

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
      warmupRepititions = config.warmupRepititions,
      repititons = config.repititions
    )

  @main
  def santaClausConfig(
      config: Config,
      @arg(short = 'a', doc = "The number of actions Santa Claus should perform (matches)")
      santaClauseActions: Int = 10000
  ) =
    runSantaClausBenchmark(
      santaClauseActions = santaClauseActions,
      writeToFile = config.writeToFile.value
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
      warmupRepititions = config.warmupRepititions,
      repititons = config.repititions
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
      warmupRepititions = config.warmupRepititions,
      repititons = config.repititions
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
      warmupRepititions = config.warmupRepititions,
      repititons = config.repititions
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
      warmupRepititions = config.warmupRepititions,
      repititons = config.repititions
    )

  def main(args: Array[String]): Unit =
    val config = ParserForMethods(this).runOrExit(args)
    println(config)

  // runChameneoBenchmark(
  //   maxMeetings = 5,
  //   numberOfChameneos = 3,
  //   stepBy = 1,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runSantaClausBenchmark(
  //   santaClauseActions = 10000,
  //   writeToFile = writeToFile
  // )

  // runSizeBenchmark(
  //   matches = 100,
  //   withShuffle = false,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )
  // runSizeWithGuardsBenchmark(
  //   matches = 100,
  //   withShuffle = false,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runSizeWithNoiseBenchmark(
  //   matches = 100,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runSizeWithGuardsWithNoiseBenchmark(
  //   matches = 100,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runSizeWithGuardsWithNonMatchingPayloadBenchmark(
  //   matches = 3,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runSizeBenchmark(
  //   matches = 100,
  //   withShuffle = true,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runSizeWithGuardsBenchmark(
  //   matches = 100,
  //   withShuffle = true,
  //   writeToFile = writeToFile,
  //   warmupRepititions = warmupRepititions,
  //   repititons = repititons
  // )

  // runPingPongBenchmark(maxHits = 10000, writeToFile = writeToFile)
