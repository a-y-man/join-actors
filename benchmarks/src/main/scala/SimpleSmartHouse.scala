package benchmarks

import actor.*
import actor.Result.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
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

def simpleSmartHouseExample(algorithm: MatchingAlgorithm) =
  var lastNotification     = Date(0L)
  var lastMotionInBathroom = Date(0L)
  var actions              = 0
  def isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }

  def busyLoop(): Unit =
    val start = System.nanoTime()
    while System.nanoTime() - start < 500000 do ()
    ()

  def bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) =>
      busyLoop()
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
    algorithm: MatchingAlgorithm
): Future[Measurement] =
  val actor = simpleSmartHouseExample(algorithm)

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
    repetitions: Int = 10
) =

  val nullPassMsgs = simpleSmartHouseMsgsWithPreMatches(5)
  Benchmark(
    name = "SimpleSmartHouse",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      s"Null Pass ${algorithm}",
      () => measureSimpleSmartHouse(nullPassMsgs, algorithm)
    ),
    passes = rangeOfRandomMsgs map { case (msgs, n) =>
      BenchmarkPass(
        s"Processing $n number of prefix messages per match.",
        () => measureSimpleSmartHouse(msgs, algorithm)
      )
    }
  )

def runSimpleSmartHouseBenchmark(
    smartHouseActions: Int,
    maxRandomMsgs: Int,
    rndMsgsStep: Int,
    jsonFileToWrite: Path,
    pathForBenchmarkData: Path,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =
  val algorithms: List[MatchingAlgorithm] =
    List(MatchingAlgorithm.StatefulTreeBasedAlgorithm) // , MatchingAlgorithm.BruteForceAlgorithm)

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

  write(
    jsonFileToWrite,
    msgsAsJson
  )
  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val m = simpleSmartHouseBenchmark(
      rangeOfRandomMsgs,
      algorithm,
      warmupRepetitions,
      repetitions
    ).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, m)
  }

  if writeToFile then saveToFile("SmartHouse", measurements, dataDir = pathForBenchmarkData)

object RunSimpleSmartHouseBenchmark extends App:
  // println(s"msgs with pre matches: ${simpleSmartHouseMsgsWithPreMatches(2)}")
  val timestamp = SimpleDateFormat("yyyy_MM_dd_HH_mm_ss").format(Date())
  val pathForJsonData =
    os.home / "Documents" / "JoinPatterns" / "rete-smarthouse-monitor" / "src" / "main" / "java" / "com" / "example" / "MonitorData" / s"${timestamp}_smartHouseData.json"

  val pathForBenchmarkData = os.pwd / "benchmarks" / "data"

  // val p: Path = os.Path(
  //   "/Documents/JoinPatterns/rete-smarthouse-monitor/src/main/java/com/example/MonitorData"
  // )

  // println(s"Current dir: ${os.pwd}")
  // println(
  //   s"Random dir: ${p}"
  // )

  runSimpleSmartHouseBenchmark(
    10,
    20,
    4,
    pathForJsonData,
    pathForBenchmarkData,
    writeToFile = false,
    warmupRepetitions = 3,
    repetitions = 5
  )
