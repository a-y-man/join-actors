package new_benchmarks.size_with_guards

import join_actors.api.*
import new_benchmarks.*
import new_benchmarks.size_with_guards.GenerateGuardedSizeMsgs.*
import org.scalacheck.Gen

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
