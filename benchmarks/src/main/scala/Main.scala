package benchmarks

import benchmarks.GenerateGuardedSizeMsgs.genGuardedSizeMsgsOfSizeN
import join_patterns.MatchingAlgorithm

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App:
  val writeToFile       = true
  val warmupRepititions = 5
  val repititons        = 10

  runSizeBenchmark(
    matches = 1000,
    withShuffle = false,
    writeToFile = writeToFile,
    warmupRepititions = warmupRepititions,
    repititons = repititons
  )
  runSizeWithGuardsBenchmark(
    matches = 1000,
    withShuffle = false,
    writeToFile = writeToFile,
    warmupRepititions = warmupRepititions,
    repititons = repititons
  )

  runSizeBenchmark(
    matches = 1000,
    withShuffle = true,
    writeToFile = writeToFile,
    warmupRepititions = warmupRepititions,
    repititons = repititons
  )
  runSizeWithGuardsBenchmark(
    matches = 1000,
    withShuffle = true,
    writeToFile = writeToFile,
    warmupRepititions = warmupRepititions,
    repititons = repititons
  )

  runPingPongBenchmark(maxHits = 10000, writeToFile = writeToFile)

  runSmartHouseBenchmark(
    smartHouseActions = 1000,
    maxRandomMsgs = 32,
    rndMsgsStep = 4,
    writeToFile = writeToFile,
    warmupRepititions = warmupRepititions,
    repititons = repititons
  )

  runBBBenchmark(
    bufferBound = 1000,
    nProdsCons = 100,
    stepBy = 10,
    writeToFile = writeToFile
  )
