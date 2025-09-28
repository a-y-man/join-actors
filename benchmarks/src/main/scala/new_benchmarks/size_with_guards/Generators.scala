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
  import GenerateGuardedSizeMsgs.*
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

val guardedSizeActors =
  Seq(guardedSize1, guardedSize2, guardedSize3, guardedSize4, guardedSize5, guardedSize6)

def sizeBenchmarkWithPayloadData(n: Int) =
  (
    s"${n}-ary guarded join pattern",
    guardedSizeActors(n - 1),
    genNMatchingMsgSeqs(n)(generateCorrectSizeMsgsWithPayloads)
  )

def sizeBenchmarkWithNoisyPayloadData(n: Int) =
  (nNoiseMsgs: Int) =>
    (
      s"${n}-ary guarded join pattern",
      guardedSizeActors(n - 1),
      genMsgsWithPayloadWithNoise(n)(nNoiseMsgs)(generateCorrectSizeMsgsWithPayloads)
    )

def sizeBenchmarkWithNonMatchingPayloadData(n: Int) =
  (nNonMatchingMsgs: Int) =>
    (
      s"${n}-ary guarded join pattern",
      guardedSizeActors(n - 1),
      genSizeMsgsWithPayloadWithNonMatchingPayload(n)(nNonMatchingMsgs)(
        generateCorrectSizeMsgsWithPayloads
      )(genNNonMatchingMsgs(n))
    )
