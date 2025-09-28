package new_benchmarks.size

import join_actors.api.*
import new_benchmarks.*

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

import SizeMsg.*

def size1(matcher: MatcherFactory) =
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
    }(matcher)
  }

def size2(matcher: MatcherFactory) =
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
    }(matcher)
  }

def size3(matcher: MatcherFactory) =
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
    }(matcher)
  }

def size4(matcher: MatcherFactory) =
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
    }(matcher)
  }

def size5(matcher: MatcherFactory) =
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
    }(matcher)
  }

def size6(matcher: MatcherFactory) =
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
    }(matcher)
  }

def generateSizeMsgs(n: Int): Vector[SizeMsg] =
  val msgs = Vector(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
  msgs.take(n)

def genMsgsNoPayloadWithNoise(patSize: Int)(nRandomMsgs: Int)(genMsg: Int => Vector[SizeMsg])(
    matches: Int
) =
  val noise = Vector.fill(nRandomMsgs)(XX())
  val correctMsgs = genMsg(patSize)
  val matchSeqWithNoise = intercalateCorrectMsgs(correctMsgs, noise)
  Vector.fill(matches)(matchSeqWithNoise).flatten

val sizeActors = Seq(size1, size2, size3, size4, size5, size6)

def sizeBenchmarks(n: Int) =
  (s"${n}-ary join pattern", sizeActors(n - 1), genNMatchingMsgSeqs(n)(generateSizeMsgs))

def sizeBenchmarksWithNoise(n: Int) =
  (noiseMessages : Int) =>
    (s"${n}-ary join pattern", sizeActors(n - 1), genMsgsNoPayloadWithNoise(n)(noiseMessages)(generateSizeMsgs))
