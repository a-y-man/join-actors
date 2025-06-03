package old_benchmarks

import join_actors.api.*
import join_patterns.matching.MatchingAlgorithm.WhileLazyAlgorithm
import org.scalacheck.Gen
import os.Path

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Random

enum SizeMsg:
  case A()
  case B()
  case C()
  case D()
  case E()
  case F()
  case G()
  case H()
  case I()
  case J()
  case XX()
  case Terminate()

def size1(algorithm: MatchingAlgorithm) =
  import SizeMsg.*
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive { (_: ActorRef[SizeMsg]) =>
      {
        case A() =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size2(algorithm: MatchingAlgorithm) =
  import SizeMsg.*
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B()) =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size3(algorithm: MatchingAlgorithm) =
  import SizeMsg.*
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C()) =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size4(algorithm: MatchingAlgorithm) =
  import SizeMsg.*
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D()) =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size5(algorithm: MatchingAlgorithm) =
  import SizeMsg.*
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E()) =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size6(algorithm: MatchingAlgorithm) =
  import SizeMsg.*
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E(), F()) =>
          matches += 1
          Continue
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def generateSizeMsgs(n: Int): Vector[SizeMsg] =
  import SizeMsg.*
  val msgs = Vector(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
  msgs.take(n)

def genMsgsNoPayloadWithNoise(patSize: Int)(nRandomMsgs: Int)(genMsg: Int => Vector[SizeMsg])(
    matches: Int
) =
  import SizeMsg.*
  val noise             = Vector.fill(nRandomMsgs)(XX())
  val correctMsgs       = genMsg(patSize)
  val matchSeqWithNoise = intercalateCorrectMsgs(correctMsgs, noise)
  Vector.fill(matches)(matchSeqWithNoise).flatten

val sizeBenchmarks = Seq(
  ("1-ary join pattern", size1, genNMatchingMsgSeqs(1)(generateSizeMsgs)),
  ("2-ary join pattern", size2, genNMatchingMsgSeqs(2)(generateSizeMsgs)),
  ("3-ary join pattern", size3, genNMatchingMsgSeqs(3)(generateSizeMsgs)),
  ("4-ary join pattern", size4, genNMatchingMsgSeqs(4)(generateSizeMsgs)),
  ("5-ary join pattern", size5, genNMatchingMsgSeqs(5)(generateSizeMsgs)),
  ("6-ary join pattern", size6, genNMatchingMsgSeqs(6)(generateSizeMsgs))
)

val sizeBenchmarksWithNoise = Seq(
  ("1-ary join pattern", size1, genMsgsNoPayloadWithNoise(1)(100)(generateSizeMsgs)),
  ("2-ary join pattern", size2, genMsgsNoPayloadWithNoise(2)(100)(generateSizeMsgs)),
  ("3-ary join pattern", size3, genMsgsNoPayloadWithNoise(3)(100)(generateSizeMsgs)),
  ("4-ary join pattern", size4, genMsgsNoPayloadWithNoise(4)(100)(generateSizeMsgs)),
  ("5-ary join pattern", size5, genMsgsNoPayloadWithNoise(5)(100)(generateSizeMsgs)),
  ("6-ary join pattern", size6, genMsgsNoPayloadWithNoise(6)(100)(generateSizeMsgs))
)

def measureSize(
    matches: Int,
    msgs: Seq[SizeMsg],
    sizeAct: MatchingAlgorithm => Actor[SizeMsg, (Long, Int)],
    algorithm: MatchingAlgorithm
) =
  import SizeMsg.*
  val actor              = sizeAct(algorithm)
  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    msgs.foreach(actorRef ! _)

    actorRef ! Terminate()

    val (endTime, numMatches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, numMatches)
  }

def measureSizeWithNoise(
    msgs: Seq[SizeMsg],
    sizeAct: MatchingAlgorithm => Actor[SizeMsg, (Long, Int)],
    algorithm: MatchingAlgorithm
) =
  import SizeMsg.*
  val actor              = sizeAct(algorithm)
  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    msgs.foreach(actorRef ! _)

    actorRef ! Terminate()

    val (endTime, numMatches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, numMatches)
  }

def sizeBenchmark(
    matches: Int,
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass = measureSize(
    matches,
    genNMatchingMsgSeqs(5)(generateSizeMsgs)(matches),
    size5,
    algorithm
  )
  Benchmark(
    name = "Pattern Size without Guards",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarks.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSize(matches, msgs(matches), sizeAct, algorithm)
      )
    }
  )

def sizeWithNoiseBenchmark(
    matches: Int,
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPass =
    measureSizeWithNoise(
      genMsgsNoPayloadWithNoise(5)(10)(generateSizeMsgs)(matches),
      size5,
      algorithm
    )
  Benchmark(
    name = "Pattern Size with Noise",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarksWithNoise.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithNoise(msgs(matches), sizeAct, algorithm)
      )
    }
  )

def runSizeBenchmark(
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
      sizeBenchmark(matches, algorithm, warmupRepetitions, repetitions).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then saveToFile("Size", measurements, outputDataDir)

def runSizeWithNoiseBenchmark(
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
      sizeWithNoiseBenchmark(matches, algorithm, warmupRepetitions, repetitions)
        .run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then saveToFile("SizeWithNoise", measurements, outputDataDir)
