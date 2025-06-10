package old_benchmarks

import join_actors.api.*
import join_patterns.matching.MatchingAlgorithm.*
import os.*

import java.text.SimpleDateFormat
import java.time.Duration as JDuration
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.*

def simpleSmartHouseExample(algorithm: MatchingAlgorithm, withHeavyGuard: Boolean) =
  var lastNotification     = Date(0L)
  var lastMotionInBathroom = Date(0L)
  var actions              = 0
  def isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }

  def busyLoop(): Unit =
    val start = System.nanoTime()
    while System.nanoTime() - start < 100000 do ()
    ()

  def bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) =>
      if withHeavyGuard then busyLoop()
      isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

  Actor[Action, (Long, Int)] {
    receive { (selfRef: ActorRef[Action]) =>
      // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
      {
        case (
              Motion(_: Int, mStatus: Boolean, mRoom: String, t0: Date),
              AmbientLight(_: Int, value: Int, alRoom: String, t1: Date),
              Light(_: Int, lStatus: Boolean, lRoom: String, t2: Date)
            )
            if bathroomOccupied(
              List(t0, t1, t2),
              List(mRoom, lRoom, alRoom),
              mStatus,
              lStatus,
              value
            ) =>
          lastNotification = Date()
          lastMotionInBathroom = lastNotification
          actions += 1
          Continue
        case ShutOff() =>
          Stop((System.currentTimeMillis(), actions))
      }
    }(algorithm)
  }

def simpleSmartHouseMsgsWithPreMatches(n: Int): Vector[Action] =
  val pat1MatchPrefix = Vector(
    Motion(0, true, "bathroom"),
    AmbientLight(0, 30, "bathroom")
  )
  val pat1MatchSuffix = Light(0, false, "bathroom")

  if n == 0 then pat1MatchPrefix :+ pat1MatchSuffix
  else Vector.fill(n)(pat1MatchPrefix).flatten :+ pat1MatchSuffix

def measureSimpleSmartHouse(
    msgs: Vector[Action],
    algorithm: MatchingAlgorithm,
    withHeavyGuard: Boolean = false
): Future[Measurement] =
  val actor = simpleSmartHouseExample(algorithm, withHeavyGuard)

  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    msgs.foreach(actorRef ! _)

    actorRef ! ShutOff()

    val (endTime, matches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }

def simpleSmartHouseBenchmark(
    rangeOfRandomMsgs: Vector[(Vector[Action], Int)],
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    withHeavyGuard: Boolean = false
) =

  val nullPassMsgs = simpleSmartHouseMsgsWithPreMatches(5)
  Benchmark(
    name = "SimpleSmartHouse",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      s"Null Pass ${algorithm}",
      () => measureSimpleSmartHouse(nullPassMsgs, algorithm, withHeavyGuard)
    ),
    passes = rangeOfRandomMsgs map { case (msgs, n) =>
      BenchmarkPass(
        s"Processing $n number of prefix messages per match.",
        () => measureSimpleSmartHouse(msgs, algorithm, withHeavyGuard)
      )
    }
  )

def runSimpleSmartHouseBenchmark(
    smartHouseActions: Int,
    maxRandomMsgs: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    withHeavyGuard: Boolean = false,
    rndMsgsStep: Int,
    pathToJsonDataFile: Path,
    pathForBenchmarkData: Path
) =
  val algorithms: List[MatchingAlgorithm] =
    List(
//      BruteForceAlgorithm,
//      StatefulTreeBasedAlgorithm,
//      MutableStatefulAlgorithm,
//      LazyMutableAlgorithm,
//      WhileEagerAlgorithm,
//      EagerParallelAlgorithm(2),
//      EagerParallelAlgorithm(4),
//      EagerParallelAlgorithm(6),
//      EagerParallelAlgorithm(8),
      WhileLazyAlgorithm,
//      LazyParallelAlgorithm(2),
//      LazyParallelAlgorithm(4),
//      LazyParallelAlgorithm(6),
//      LazyParallelAlgorithm(8)
    )

  val rangeOfRandomMsgs =
    Vector((0 to maxRandomMsgs by rndMsgsStep)*) map { n =>
      val allMsgsForNRndMsgs =
        Vector.fill(smartHouseActions)(simpleSmartHouseMsgsWithPreMatches(n))

      // println(s"allMsgsForNRndMsgs: ${allMsgsForNRndMsgs.size}")
      (allMsgsForNRndMsgs.flatten, n)
    } map { case (msgs, n) =>
      val updateMsgIds =
        msgs.zipWithIndex.map { case (msg, i) =>
          msg match
            case Motion(_, _, _, _)       => msg.asInstanceOf[Motion].copy(id = i)
            case AmbientLight(_, _, _, _) => msg.asInstanceOf[AmbientLight].copy(id = i)
            case Light(_, _, _, _)        => msg.asInstanceOf[Light].copy(id = i)
            case Contact(_, _, _, _)      => msg.asInstanceOf[Contact].copy(id = i)
            case Consumption(_, _, _)     => msg.asInstanceOf[Consumption].copy(id = i)
            case HeatingF(_, _, _)        => msg.asInstanceOf[HeatingF].copy(id = i)
            case DoorBell(_, _)           => msg.asInstanceOf[DoorBell].copy(id = i)
            case ShutOff()                => msg
        }

      (updateMsgIds, n)
    }

  val msgsAsJson = rangeOfRandomMsgs
    .map { case (msgs, n) =>
      val msgsToJson = msgs map { msg => ActionToJsonFormatter.toJson(msg) }
      s"""
        |{
        |  "number_of_messages": ${msgs.size},
        |  "number_of_random_messages": $n,
        |  "messages": ${msgsToJson.mkString("[", ",", "]")}
        |}""".stripMargin
    }
    .mkString("[", ",", "]")

  val timestamp    = SimpleDateFormat("yyyy_MM_dd_HH_mm_ss").format(Date())
  val jsonDataFile = pathToJsonDataFile / s"${timestamp}_smartHouseData.json"

  os.makeDir.all(pathToJsonDataFile)
  write(jsonDataFile, msgsAsJson)

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val m = simpleSmartHouseBenchmark(
      rangeOfRandomMsgs,
      algorithm,
      warmupRepetitions,
      repetitions,
      withHeavyGuard
    ).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, m)
  }

  if writeToFile then
    if !withHeavyGuard then
      saveToFile("JoinActorsSmartHouse", measurements, dataDir = pathForBenchmarkData)
    else
      saveToFile(
        "JoinActorsSmartHouseWithHeavyGuards",
        measurements,
        dataDir = pathForBenchmarkData
      )
