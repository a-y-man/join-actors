package benchmarks

import join_patterns.MatchingAlgorithm

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
  // def run(args: Array[String]): Unit =
  runSmartHouseBenchmark(
    smartHouseActions = 10,
    maxRandomMsgs = 32,
    rndMsgsStep = 4,
    writeToFile = true
  )
  // runPingPongBenchmark(10)
  // runSizeBenchmark(1000)
