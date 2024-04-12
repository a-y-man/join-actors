package benchmarks

import actor.*
import ch.qos.logback.core.pattern.parser.Parser
import join_patterns.MatchingAlgorithm
import join_patterns.receive_
import mainargs.Flag
import mainargs.ParserForMethods
import mainargs.arg
import mainargs.main
import org.scalacheck.Gen

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Random

sealed trait SizeMsg
case class A()         extends SizeMsg
case class B()         extends SizeMsg
case class C()         extends SizeMsg
case class D()         extends SizeMsg
case class E()         extends SizeMsg
case class F()         extends SizeMsg
case class G()         extends SizeMsg
case class H()         extends SizeMsg
case class I()         extends SizeMsg
case class J()         extends SizeMsg
case class NoiseA()    extends SizeMsg
case class NoiseB()    extends SizeMsg
case class NoiseC()    extends SizeMsg
case class NoiseD()    extends SizeMsg
case class NoiseE()    extends SizeMsg
case class NoiseF()    extends SizeMsg
case class NoiseG()    extends SizeMsg
case class NoiseH()    extends SizeMsg
case class NoiseI()    extends SizeMsg
case class NoiseJ()    extends SizeMsg
case class Terminate() extends SizeMsg

def size1(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case A() =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size2(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size3(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size4(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size5(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size6(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E(), F()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size7(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E(), F(), G()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size8(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E(), F(), G(), H()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size9(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E(), F(), G(), H(), I()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def size10(algorithm: MatchingAlgorithm) =
  var matches = 0
  Actor[SizeMsg, (Long, Int)] {
    receive_ { (_: ActorRef[SizeMsg]) =>
      {
        case (A(), B(), C(), D(), E(), F(), G(), H(), I(), J()) =>
          matches += 1
          Next()
        case Terminate() =>
          Stop((System.currentTimeMillis(), matches))
      }
    }(algorithm)
  }

def generateSizeMsgs(n: Int, isShuffled: Boolean = false): Vector[SizeMsg] =
  val msgs  = Vector(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
  val nMsgs = msgs.take(n)
  if isShuffled then Random.shuffle(nMsgs) else nMsgs

lazy val sizeBenchmarks = Seq(
  ("1-ary join pattern", size1, generateSizeMsgs(1)),
  ("2-ary join pattern", size2, generateSizeMsgs(2)),
  ("3-ary join pattern", size3, generateSizeMsgs(3)),
  ("4-ary join pattern", size4, generateSizeMsgs(4)),
  ("5-ary join pattern", size5, generateSizeMsgs(5)),
  ("6-ary join pattern", size6, generateSizeMsgs(6)),
  ("7-ary join pattern", size7, generateSizeMsgs(7)),
  ("8-ary join pattern", size8, generateSizeMsgs(8)),
  ("9-ary join pattern", size9, generateSizeMsgs(9)),
  ("10-ary join pattern", size10, generateSizeMsgs(10))
)

lazy val sizeBenchmarksShuffledMsgs = Seq(
  ("1-ary join pattern", size1, generateSizeMsgs(1, true)),
  ("2-ary join pattern", size2, generateSizeMsgs(2, true)),
  ("3-ary join pattern", size3, generateSizeMsgs(3, true)),
  ("4-ary join pattern", size4, generateSizeMsgs(4, true)),
  ("5-ary join pattern", size5, generateSizeMsgs(5, true)),
  ("6-ary join pattern", size6, generateSizeMsgs(6, true)),
  ("7-ary join pattern", size7, generateSizeMsgs(7, true)),
  ("8-ary join pattern", size8, generateSizeMsgs(8, true)),
  ("9-ary join pattern", size9, generateSizeMsgs(9, true)),
  ("10-ary join pattern", size10, generateSizeMsgs(10, true))
)

def measureSize(
    matches: Int,
    msgs: Seq[SizeMsg],
    sizeAct: MatchingAlgorithm => Actor[SizeMsg, (Long, Int)],
    algorithm: MatchingAlgorithm
) =
  val actor              = sizeAct(algorithm)
  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    for _ <- 1 to matches do msgs.foreach(actorRef ! _)

    actorRef ! Terminate()

    val (endTime, numMatches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, numMatches)
  }

def sizeBenchmark(matches: Int, isShuffled: Boolean, algorithm: MatchingAlgorithm) =

  val nullPass = measureSize(matches, generateSizeMsgs(5, isShuffled), size5, algorithm)
  Benchmark(
    name = "Pattern Size without Guards",
    algorithm = algorithm,
    warmupIterations = 5,
    iterations = 10,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes =
      if isShuffled then
        sizeBenchmarksShuffledMsgs.map { case (name, sizeAct, msgs) =>
          BenchmarkPass(
            name,
            () => measureSize(matches, msgs, sizeAct, algorithm)
          )
        }
      else
        sizeBenchmarks.map { case (name, sizeAct, msgs) =>
          BenchmarkPass(
            name,
            () => measureSize(matches, msgs, sizeAct, algorithm)
          )
        }
  )

def runSizeBenchmark(
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
    val measurement = sizeBenchmark(matches, withShuffle, algorithm).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, measurement)
  }

  if writeToFile then saveToFile("Size", measurements)

// object SizeBenchmark:
//   @main
//   def run(
//       @arg(short = 'w', doc = "write to file")
//       writeToFile: Boolean = false,
//       @arg(short = 'm', doc = "maximal number of matches per iteration")
//       maxMatches: Int = 1000,
//       @arg(short = 's', doc = "shuffle the messages before sending")
//       bool: Flag
//   ): Unit =
//     println(s"Running Size benchmark with $maxMatches matches per iteration ")

//   def main(args: Array[String]): Unit =
//     ParserForMethods(this).runOrExit(args)
