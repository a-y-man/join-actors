package old_benchmarks.size_with_guards

import old_benchmarks.utils.*
import join_actors.api.*
import org.scalacheck.Gen
import os.Path

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Random
import old_benchmarks.Benchmark
import old_benchmarks.BenchmarkPass
import old_benchmarks.saveToFile
import old_benchmarks.Measurement

import old_benchmarks.utils.GenerateGuardedSizeMsgs.*

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

def guardedSize1(matcher: MatcherFactory) =
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
    }(matcher)
  }

def guardedSize2(matcher: MatcherFactory) =
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
    }(matcher)
  }

def guardedSize3(matcher: MatcherFactory) =
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
    }(matcher)
  }

def guardedSize4(matcher: MatcherFactory) =
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
    }(matcher)
  }

def guardedSize5(matcher: MatcherFactory) =
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
    }(matcher)
  }

def guardedSize6(matcher: MatcherFactory) =
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
    }(matcher)
  }

def generateCorrectSizeMsgsWithPayloads(n: Int): Vector[GuardedSizeMsg] =
  import old_benchmarks.utils.GenerateGuardedSizeMsgs.*
  genGuardedSizeMsgsOfSizeN(n).get

def genSizeMsgsWithPayloadWithNonMatchingPayload(patSize: Int)(nRandomMsgs: Int)(
    correctMsgsGen: Int => Vector[GuardedSizeMsg]
)(wrongMsgsGen: Int => Vector[GuardedSizeMsg])(
    matches: Int
) =
  import GuardedSizeMsg.*
  val badMsgs = wrongMsgsGen(nRandomMsgs)
  val correctMsgs = correctMsgsGen(patSize)
  val matchingAndNonMatchingSeqs = intercalateCorrectMsgs(correctMsgs, badMsgs)
  Vector.fill(matches)(matchingAndNonMatchingSeqs).flatten

def genMsgsWithPayloadWithNoise(patSize: Int)(nRandomMsgs: Int)(
    correctMsgsGen: Int => Vector[GuardedSizeMsg]
)(matches: Int) =
  import GuardedSizeMsg.*
  val noise = Vector.fill(nRandomMsgs)(XX())
  val correctMsgs = correctMsgsGen(patSize)
  val matchingMsgsAndNoiseSeqs = intercalateCorrectMsgs(correctMsgs, noise)
  Vector.fill(matches)(matchingMsgsAndNoiseSeqs).flatten

val N_NOISE_MSGS = 100
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
    sizeAct: MatcherFactory => Actor[GuardedSizeMsg, (Long, Int)],
    matcher: MatcherFactory
) =
  import GuardedSizeMsg.*
  val actor = sizeAct(matcher)
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
    matcher: MatcherFactory,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithGuards(
      genNMatchingMsgSeqs(3)(generateCorrectSizeMsgsWithPayloads)(matches),
      guardedSize3,
      matcher
    )
  Benchmark(
    name = "Pattern Size with Guards",
    matcher = matcher,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(msgs(matches), sizeAct, matcher)
      )
    }
  )

def sizeWithGuardsWithNoiseBenchmark(
    matches: Int,
    matcher: MatcherFactory,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithGuards(
      genMsgsWithPayloadWithNoise(3)(10)(generateCorrectSizeMsgsWithPayloads)(matches),
      guardedSize3,
      matcher
    )
  Benchmark(
    name = "Pattern Size with Guards with Noise",
    matcher = matcher,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithNoisyPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(msgs(matches), sizeAct, matcher)
      )
    }
  )

def sizeWithGuardsWithNonMatchingPayloadDataBenchmark(
    matches: Int,
    matcher: MatcherFactory,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithGuards(
      genSizeMsgsWithPayloadWithNonMatchingPayload(3)(10)(generateCorrectSizeMsgsWithPayloads)(
        genNNonMatchingMsgs(3)
      )(matches),
      guardedSize3,
      matcher
    )
  Benchmark(
    name = "Pattern Size with Guards with Non-matching Payload",
    matcher = matcher,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithNonMatchingPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(msgs(matches), sizeAct, matcher)
      )
    }
  )

val matchers: List[MatcherFactory] =
  List(
    StatefulTreeMatcher,
    MutableStatefulMatcher,
    LazyMutableMatcher,
    WhileLazyMatcher,
    FilteringWhileMatcher,
    WhileEagerMatcher,
    ArrayWhileMatcher,
    EagerParallelMatcher(2),
    EagerParallelMatcher(4),
    EagerParallelMatcher(6),
    EagerParallelMatcher(8),
    LazyParallelMatcher(2),
    LazyParallelMatcher(4),
    LazyParallelMatcher(6),
    LazyParallelMatcher(8),
    FilteringParallelMatcher(2),
    FilteringParallelMatcher(4),
    FilteringParallelMatcher(6),
    FilteringParallelMatcher(8),
    ArrayParallelMatcher(2),
    ArrayParallelMatcher(4),
    ArrayParallelMatcher(6),
    ArrayParallelMatcher(8)
  )

def runSizeWithGuardsBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val measurements = matchers map { matcher =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $matcher${Console.RESET}"
    )
    val measurement =
      sizeWithGuardsBenchmark(matches, matcher, warmupRepetitions, repetitions).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $matcher finished${Console.RESET}"
    )

    (matcher, measurement)
  }

  if writeToFile then saveToFile("SizeWithGuards", measurements, outputDataDir)

def runSizeWithGuardsWithNoiseBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val measurements = matchers map { matcher =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $matcher${Console.RESET}"
    )
    val measurement =
      sizeWithGuardsWithNoiseBenchmark(matches, matcher, warmupRepetitions, repetitions)
        .run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $matcher finished${Console.RESET}"
    )

    (matcher, measurement)
  }

  if writeToFile then saveToFile("SizeWithGuardsWithNoise", measurements, outputDataDir)

def runSizeWithGuardsWithNonMatchingPayloadBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val measurements = matchers map { matcher =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $matcher${Console.RESET}"
    )
    val measurement =
      sizeWithGuardsWithNonMatchingPayloadDataBenchmark(
        matches,
        matcher,
        warmupRepetitions,
        repetitions
      )
        .run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $matcher finished${Console.RESET}"
    )

    (matcher, measurement)
  }

  if writeToFile then
    saveToFile("SizeWithGuardsWithNonMatchingPayload", measurements, outputDataDir)
