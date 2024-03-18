package benchmarks

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

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
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case A() => Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size2(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size3(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size4(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size5(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size6(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E(), F()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size7(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size8(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size9(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def size10(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I(), J()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

def generateSizeMsgs(n: Int): Vector[SizeMsg] =
  val msgs = Vector(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
  (0 until n).map(msgs).toVector

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

def measureSize(
    matches: Int,
    msgs: Seq[SizeMsg],
    sizeAct: MatchingAlgorithm => Actor[SizeMsg, Long],
    algorithm: MatchingAlgorithm
) =
  val actor              = sizeAct(algorithm)
  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    for _ <- 1 to matches do msgs.foreach(actorRef ! _)

    actorRef ! Terminate()

    val endTime = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }

def sizeBenchmark(matches: Int, algorithm: MatchingAlgorithm) =

  val nullPass = measureSize(matches, generateSizeMsgs(1), size1, algorithm)
  Benchmark(
    name = "Pattern Size",
    algorithm = algorithm,
    warmupIterations = 5,
    iterations = 10,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = sizeBenchmarks.map { case (name, sizeAct, msgs) =>
      BenchmarkPass(
        name,
        () => measureSize(matches, msgs, sizeAct, algorithm)
      )
    }
  )

def runSizeBenchmark(
    matches: Int,
    writeToFile: Boolean = false,
    algorithms: List[MatchingAlgorithm] =
      List(MatchingAlgorithm.StatefulTreeBasedAlgorithm, MatchingAlgorithm.BruteForceAlgorithm)
) =
  algorithms foreach { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    sizeBenchmark(matches, algorithm).run(writeToFile)
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )
  }
