package benchmarks

import join_patterns.MatchingAlgorithm
import join_patterns.examples.runBBBenchmark

import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class BenchmarkConfig(
    val benchmarkName: String,
    val algorithm: MatchingAlgorithm,
    val warmupIters: Int,
    val iterations: Int
)

val algorithms = Seq(
  MatchingAlgorithm.BruteForceAlgorithm,
  MatchingAlgorithm.StatefulTreeBasedAlgorithm
)

lazy val benchmarks = Seq(
  "SmartHouse",
  "SantaClaus",
  "BoundedBuffer",
  "Chameneos",
  "Size",
  "PingPong"
)

object Main extends App:
  val writeToFile = true

  runSmartHouseBenchmark(
    smartHouseActions = 4,
    maxRandomMsgs = 64,
    rndMsgsStep = 4,
    writeToFile = writeToFile
  )

  runPingPongBenchmark(maxHits = 10000, writeToFile = writeToFile)

  runSizeBenchmark(matches = 1000, withShuffle = true, writeToFile = writeToFile)

  runSizeWithGuardsBenchmark(
    matches = 10000,
    withShuffle = true,
    writeToFile = writeToFile
  )

  runBBBenchmark(
    bufferBound = 1000,
    nProdsCons = 100,
    stepBy = 5,
    writeToFile = writeToFile
  )
