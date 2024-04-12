package benchmarks

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
import join_patterns.receive_
import org.scalacheck.Gen

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
  case Terminate()

def guardedSize1(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case A(x) if x >= 0 =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize2(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y)) if x == y =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize3(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z)) if x == y && y == z =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize4(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w)) if x == y && y == z && z == w =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize5(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a)) if x == y && y == z && z == w && w == a =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize6(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a), F(b))
            if x == y && y == z && z == w && w == a && a == b =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize7(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a), F(b), G(c))
            if x == y && y == z && z == w && w == a && a == b && b == c =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize8(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a), F(b), G(c), H(d))
            if x == y && y == z && z == w && w == a && a == b && b == c && c == d =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize9(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a), F(b), G(c), H(d), I(e))
            if x == y && y == z && z == w && w == a && a == b && b == c && c == d && d == e =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def guardedSize10(algorithm: MatchingAlgorithm) =
  import GuardedSizeMsg.*
  var matches = 0
  Actor[GuardedSizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[GuardedSizeMsg]) =>
      {
        case (A(x), B(y), C(z), D(w), E(a), F(b), G(c), H(d), I(e), J(f))
            if x == y && y == z && z == w && w == a && a == b && b == c && c == d && d == e && e == f =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def generateSizeMsgsWithPayloads(n: Int): Vector[GuardedSizeMsg] =
  import GenerateGuardedSizeMsgs.*
  genGuardedSizeMsgsOfSizeN(n).get

lazy val sizeBenchmarkWithPayloadData =
  Seq(
    (
      "1-ary conditional join pattern",
      guardedSize1,
      genNMatchingMsgSeqs(1)(generateSizeMsgsWithPayloads)
    ),
    (
      "2-ary conditional join pattern",
      guardedSize2,
      genNMatchingMsgSeqs(2)(generateSizeMsgsWithPayloads)
    ),
    (
      "3-ary conditional join pattern",
      guardedSize3,
      genNMatchingMsgSeqs(3)(generateSizeMsgsWithPayloads)
    ),
    (
      "4-ary conditional join pattern",
      guardedSize4,
      genNMatchingMsgSeqs(4)(generateSizeMsgsWithPayloads)
    ),
    (
      "5-ary conditional join pattern",
      guardedSize5,
      genNMatchingMsgSeqs(5)(generateSizeMsgsWithPayloads)
    ),
    (
      "6-ary conditional join pattern",
      guardedSize6,
      genNMatchingMsgSeqs(6)(generateSizeMsgsWithPayloads)
    ),
    (
      "7-ary conditional join pattern",
      guardedSize7,
      genNMatchingMsgSeqs(7)(generateSizeMsgsWithPayloads)
    ),
    (
      "8-ary conditional join pattern",
      guardedSize8,
      genNMatchingMsgSeqs(8)(generateSizeMsgsWithPayloads)
    ),
    (
      "9-ary conditional join pattern",
      guardedSize9,
      genNMatchingMsgSeqs(9)(generateSizeMsgsWithPayloads)
    ),
    (
      "10-ary conditional join pattern",
      guardedSize1,
      genNMatchingMsgSeqs(10)(generateSizeMsgsWithPayloads)
    )
  )

def measureSizeWithGuards(
    matches: Int,
    msgs: Seq[GuardedSizeMsg],
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

def sizeWithGuardsBenchmark(matches: Int, isShuffled: Boolean, algorithm: MatchingAlgorithm) =

  val nullPass =
    measureSizeWithGuards(
      matches,
      genNMatchingMsgSeqs(5)(generateSizeMsgsWithPayloads)(matches)(isShuffled),
      guardedSize5,
      algorithm
    )
  Benchmark(
    name = "Pattern Size with Guards",
    algorithm = algorithm,
    warmupIterations = 5,
    iterations = 10,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarkWithPayloadData.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSizeWithGuards(matches, msgs(matches)(isShuffled), sizeAct, algorithm)
      )
    }
  )

def runSizeWithGuardsBenchmark(
    matches: Int,
    withShuffle: Boolean = false,
    writeToFile: Boolean = false
) =
  val algorithms: List[MatchingAlgorithm] =
    List(MatchingAlgorithm.StatefulTreeBasedAlgorithm, MatchingAlgorithm.BruteForceAlgorithm)

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val measurement = sizeWithGuardsBenchmark(matches, withShuffle, algorithm).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then
    if withShuffle then saveToFile("SizeWithGuardsWithShuffle", measurements)
    else saveToFile("SizeWithGuards", measurements)
