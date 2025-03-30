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

def size1(algorithm: MatchingAlgorithm) =
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
  val msgs = Vector(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
  msgs.take(n)

def genMsgsNoPayloadWithNoise(patSize: Int)(nRandomMsgs: Int)(genMsg: Int => Vector[SizeMsg])(
  matches: Int
) =
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