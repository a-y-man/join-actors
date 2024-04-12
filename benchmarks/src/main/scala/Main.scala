package benchmarks

import join_patterns.MatchingAlgorithm

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App:
  val writeToFile = true

  runSizeBenchmark(matches = 5, withShuffle = false, writeToFile = writeToFile)

  runSizeWithGuardsBenchmark(
    matches = 5,
    withShuffle = true,
    writeToFile = writeToFile
  )

  runPingPongBenchmark(maxHits = 10000, writeToFile = writeToFile)

  runSmartHouseBenchmark(
    smartHouseActions = 10,
    maxRandomMsgs = 32,
    rndMsgsStep = 4,
    writeToFile = writeToFile
  )

  runBBBenchmark(
    bufferBound = 100,
    nProdsCons = 10,
    stepBy = 1,
    writeToFile = writeToFile
  )
