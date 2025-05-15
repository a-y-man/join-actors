package old_benchmarks

import old_benchmarks.GenerateGuardedSizeMsgs.genNNonMatchingMsgs
import join_actors.api.*
import join_patterns.matching.MatchingAlgorithm.WhileLazyAlgorithm
import org.scalacheck.Gen
import os.Path

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Random

enum GuardedSizeMsg:
  case A(x: Int)
  case B(x: Int)
  case C(x: Int)
  case D(x: Int)
  case E(x: Int)
  case F(x: Int)
  case G(x: Int)
  case H(x: Int)
  case I(x: Int)
  case J(x: Int)
  case XX()
  case Terminate()

def guardedSize1(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case A(x) if x >= 0 =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize2(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y)) if x == y =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize3(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z)) if x == y && y == z =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize4(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w)) if x == y && y == z && z == w =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize5(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a)) if x == y && y == z && z == w && w == a =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize6(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a), F(b))
            if x == y && y == z && z == w && w == a && a == b =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def generateCorrectSizeMsgsWithPayloads(n: Int): Vector[GuardedSizeMsg] =
  import GenerateGuardedSizeMsgs.*
  genGuardedSizeMsgsOfSizeN(n).get

def genSizeMsgsWithPayloadWithNonMatchingPayload(patSize: Int)(nRandomMsgs: Int)(
    correctMsgsGen: Int => Vector[GuardedSizeMsg]
)(wrongMsgsGen: Int => Vector[GuardedSizeMsg])(
    matches: Int
) =
  import GuardedSizeMsg.*
  val badMsgs                    = wrongMsgsGen(nRandomMsgs)
  val correctMsgs                = correctMsgsGen(patSize)
  val matchingAndNonMatchingSeqs = intercalateCorrectMsgs(correctMsgs, badMsgs)
  Vector.fill(matches)(matchingAndNonMatchingSeqs).flatten

def genMsgsWithPayloadWithNoise(patSize: Int)(nRandomMsgs: Int)(
    correctMsgsGen: Int => Vector[GuardedSizeMsg]
)(matches: Int) =
  import GuardedSizeMsg.*
  val noise                    = Vector.fill(nRandomMsgs)(XX())
  val correctMsgs              = correctMsgsGen(patSize)
  val matchingMsgsAndNoiseSeqs = intercalateCorrectMsgs(correctMsgs, noise)
  Vector.fill(matches)(matchingMsgsAndNoiseSeqs).flatten

val N_NOISE_MSGS        = 100
val N_NON_MATCHING_MSGS = 10

val sizeBenchmarkWithPayloadData =
  Seq(
    (
      "1-ary conditional join pattern",
      guardedSize1,
      genNMatchingMsgSeqs(1)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "2-ary conditional join pattern",
      guardedSize2,
      genNMatchingMsgSeqs(2)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "3-ary conditional join pattern",
      guardedSize3,
      genNMatchingMsgSeqs(3)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "4-ary conditional join pattern",
      guardedSize4,
      genNMatchingMsgSeqs(4)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "5-ary conditional join pattern",
      guardedSize5,
      genNMatchingMsgSeqs(5)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "6-ary conditional join pattern",
      guardedSize6,
      genNMatchingMsgSeqs(6)(generateCorrectSizeMsgsWithPayloads)
    )
  )

val sizeBenchmarkWithNoisyPayloadData =
  Seq(
    (
      "1-ary conditional join pattern",
      guardedSize1,
      genMsgsWithPayloadWithNoise(1)(N_NOISE_MSGS)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "2-ary conditional join pattern",
      guardedSize2,
      genMsgsWithPayloadWithNoise(2)(N_NOISE_MSGS)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "3-ary conditional join pattern",
      guardedSize3,
      genMsgsWithPayloadWithNoise(3)(N_NOISE_MSGS)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "4-ary conditional join pattern",
      guardedSize4,
      genMsgsWithPayloadWithNoise(4)(N_NOISE_MSGS)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "5-ary conditional join pattern",
      guardedSize5,
      genMsgsWithPayloadWithNoise(5)(N_NOISE_MSGS)(generateCorrectSizeMsgsWithPayloads)
    ),
    (
      "6-ary conditional join pattern",
      guardedSize6,
      genMsgsWithPayloadWithNoise(6)(N_NOISE_MSGS)(generateCorrectSizeMsgsWithPayloads)
    )
  )

val sizeBenchmarkWithNonMatchingPayloadData =
  Seq(
    (
      "1-ary conditional join pattern",
      guardedSize1,
      genSizeMsgsWithPayloadWithNonMatchingPayload(1)(N_NON_MATCHING_MSGS)(
        generateCorrectSizeMsgsWithPayloads
      )(
        genNNonMatchingMsgs(1)
      )
    ),
    (
      "2-ary conditional join pattern",
      guardedSize2,
      genSizeMsgsWithPayloadWithNonMatchingPayload(2)(N_NON_MATCHING_MSGS)(
        generateCorrectSizeMsgsWithPayloads
      )(
        genNNonMatchingMsgs(2)
      )
    ),
    (
      "3-ary conditional join pattern",
      guardedSize3,
      genSizeMsgsWithPayloadWithNonMatchingPayload(3)(N_NON_MATCHING_MSGS)(
        generateCorrectSizeMsgsWithPayloads
      )(
        genNNonMatchingMsgs(3)
      )
    ),
    (
      "4-ary conditional join pattern",
      guardedSize4,
      genSizeMsgsWithPayloadWithNonMatchingPayload(4)(N_NON_MATCHING_MSGS)(
        generateCorrectSizeMsgsWithPayloads
      )(
        genNNonMatchingMsgs(4)
      )
    ),
    (
      "5-ary conditional join pattern",
      guardedSize5,
      genSizeMsgsWithPayloadWithNonMatchingPayload(5)(N_NON_MATCHING_MSGS)(
        generateCorrectSizeMsgsWithPayloads
      )(
        genNNonMatchingMsgs(5)
      )
    ),
    (
      "6-ary conditional join pattern",
      guardedSize6,
      genSizeMsgsWithPayloadWithNonMatchingPayload(6)(N_NON_MATCHING_MSGS)(
        generateCorrectSizeMsgsWithPayloads
      )(
        genNNonMatchingMsgs(6)
      )
    )
  )

def measureSizeWithGuards(
    msgs: Vector[GuardedSizeMsg],
    sizeAct: MatchingAlgorithm => Actor[GuardedSizeMsg, (Long, Int)],
    algorithm: MatchingAlgorithm
) =
  import GuardedSizeMsg.*
  val actor              = sizeAct(algorithm)
  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    msgs.foreach(actorRef ! _)

    actorRef ! Terminate()

    val (endTime, numMatches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, numMatches)
  }

def sizeWithGuardsBenchmark(
    matches: Int,
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithGuards(
      genNMatchingMsgSeqs(3)(generateCorrectSizeMsgsWithPayloads)(matches),
      guardedSize3,
      algorithm
    )
  Benchmark(
    name = "Pattern Size with Guards",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(msgs(matches), sizeAct, algorithm)
      )
    }
  )

def sizeWithGuardsWithNoiseBenchmark(
    matches: Int,
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithGuards(
      genMsgsWithPayloadWithNoise(3)(10)(generateCorrectSizeMsgsWithPayloads)(matches),
      guardedSize3,
      algorithm
    )
  Benchmark(
    name = "Pattern Size with Guards with Noise",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithNoisyPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(msgs(matches), sizeAct, algorithm)
      )
    }
  )

def sizeWithGuardsWithNonMatchingPayloadDataBenchmark(
    matches: Int,
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithGuards(
      genSizeMsgsWithPayloadWithNonMatchingPayload(3)(10)(generateCorrectSizeMsgsWithPayloads)(
        genNNonMatchingMsgs(3)
      )(matches),
      guardedSize3,
      algorithm
    )
  Benchmark(
    name = "Pattern Size with Guards with Non-matching Payload",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithNonMatchingPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(msgs(matches), sizeAct, algorithm)
      )
    }
  )

def runSizeWithGuardsBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val algorithms: List[MatchingAlgorithm] =
    List(WhileLazyAlgorithm)

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val measurement =
      sizeWithGuardsBenchmark(matches, algorithm, warmupRepetitions, repetitions).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then saveToFile("SizeWithGuards", measurements, outputDataDir)

def runSizeWithGuardsWithNoiseBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val algorithms: List[MatchingAlgorithm] =
    List(WhileLazyAlgorithm)

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val measurement =
      sizeWithGuardsWithNoiseBenchmark(matches, algorithm, warmupRepetitions, repetitions)
        .run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then saveToFile("SizeWithGuardsWithNoise", measurements, outputDataDir)

def runSizeWithGuardsWithNonMatchingPayloadBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val algorithms: List[MatchingAlgorithm] =
    List(
      //        BruteForceAlgorithm,
//      StatefulTreeBasedAlgorithm,
//      MutableStatefulAlgorithm,
//      LazyMutableAlgorithm,
//      WhileEagerAlgorithm,
      //        EagerParallelAlgorithm(2),
      //        EagerParallelAlgorithm(4),
      //        EagerParallelAlgorithm(6),
      //        EagerParallelAlgorithm(8),
      WhileLazyAlgorithm,
      //        LazyParallelAlgorithm(2),
      //        LazyParallelAlgorithm(4),
      //        LazyParallelAlgorithm(6),
//      LazyParallelAlgorithm(8)
    )

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val measurement =
      sizeWithGuardsWithNonMatchingPayloadDataBenchmark(
        matches,
        algorithm,
        warmupRepetitions,
        repetitions
      )
        .run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then
    saveToFile("SizeWithGuardsWithNonMatchingPayload", measurements, outputDataDir)
