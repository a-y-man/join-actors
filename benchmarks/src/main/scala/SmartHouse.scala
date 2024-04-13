package benchmarks

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
import os.*

import java.util.Date
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.*

sealed trait Action
case class Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date())  extends Action
case class AmbientLight(id: Int, value: Int, room: String, timestamp: Date = Date()) extends Action
case class Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())   extends Action
case class Contact(id: Int, open: Boolean, room: String, timestamp: Date = Date())   extends Action
case class Consumption(meter_id: Int, value: Int, timestamp: Date = Date())          extends Action
case class HeatingF(id: Int, _type: String, timestamp: Date = Date())                extends Action
case class DoorBell(id: Int, timestamp: Date = Date())                               extends Action
case class ShutOff()                                                                 extends Action

def smartHouseExample(algorithm: MatchingAlgorithm) =
  var lastNotification     = Date(0L)
  var lastMotionInBathroom = Date(0L)
  var actions              = 0
  def isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }
  def bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) => isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

  def occupiedHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "front_door" && cRoom == "front_door" && mRoom1 == "entrance_hall"

  def emptyHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "entrance_hall" && cRoom == "front_door" && mRoom1 == "front_door"

  Actor[Action, (Long, Int)] {
    receive { (y: Action, selfRef: ActorRef[Action]) =>
      y match
        // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
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
          Next()
        // E5. Detect home arrival or leaving based on a particular sequence of messages, and activate the corresponding scene.
        case (
              Motion(_: Int, mStatus0: Boolean, mRoom0: String, t0: Date),
              Contact(_: Int, cStatus: Boolean, cRoom: String, t1: Date),
              Motion(_: Int, mStatus1: Boolean, mRoom1: String, t2: Date)
            )
            if occupiedHome(
              List(t0, t1, t2),
              List(mStatus0, mStatus1, cStatus),
              mRoom0,
              mRoom1,
              cRoom
            ) =>
          lastNotification = Date()
          actions += 1
          Next()
        case (
              Motion(_: Int, mStatus0: Boolean, mRoom0: String, t0: Date),
              Contact(_: Int, cStatus: Boolean, cRoom: String, t1: Date),
              Motion(_: Int, mStatus1: Boolean, mRoom1: String, t2: Date)
            )
            if emptyHome(
              List(t0, t1, t2),
              List(mStatus0, mStatus1, cStatus),
              mRoom0,
              mRoom1,
              cRoom
            ) =>
          lastNotification = Date()
          actions += 1
          Next()
        case ShutOff() =>
          Stop((System.currentTimeMillis(), actions))
    }(algorithm)
  }

def smartHouseMsgs(n: Int): Vector[Action] =
  val randomMsgs = GenerateActions.genActionsOfSizeN(n).toVector.flatten

  val correctMsgs =
    Random.nextInt(3) match
      case 0 =>
        // List of messages that trigger E1
        Vector(
          Motion(0, true, "bathroom"),
          AmbientLight(0, 30, "bathroom"),
          Light(0, false, "bathroom")
        )
      case 1 =>
        // List of messages that trigger E5.1
        Vector(
          Motion(0, true, "front_door"),
          Contact(0, true, "front_door"),
          Motion(0, true, "entrance_hall")
        )
      case 2 =>
        // List of messages that trigger E5.2
        Vector(
          Motion(0, true, "entrance_hall"),
          Contact(0, true, "front_door"),
          Motion(0, true, "front_door")
        )

  intercalateCorrectMsgs(correctMsgs, randomMsgs)

def measureSmartHouse(
    smartHouseActions: Int,
    msgs: Vector[Action],
    algorithm: MatchingAlgorithm
): Future[Measurement] =
  val actor = smartHouseExample(algorithm)

  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    for _ <- 1 to smartHouseActions do msgs.foreach(actorRef ! _)

    actorRef ! ShutOff()

    val (endTime, matches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }

def smartHouseBenchmark(
    smartHouseActions: Int,
    rangeOfRandomMsgs: Vector[(Vector[Action], Int)],
    algorithm: MatchingAlgorithm,
    warmupRepititions: Int = 5,
    repititons: Int = 10
) =

  val nullPassMsgs = smartHouseMsgs(5)
  Benchmark(
    name = "SmartHouse",
    algorithm = algorithm,
    warmupRepititions = warmupRepititions,
    repititons = repititons,
    nullPass = BenchmarkPass(
      s"Null Pass ${algorithm}",
      () => measureSmartHouse(smartHouseActions, nullPassMsgs, algorithm)
    ),
    passes = rangeOfRandomMsgs map { case (msgs, n) =>
      BenchmarkPass(
        s"$n",
        () => measureSmartHouse(smartHouseActions, msgs, algorithm)
      )
    }
  )

def runSmartHouseBenchmark(
    smartHouseActions: Int,
    maxRandomMsgs: Int,
    rndMsgsStep: Int,
    writeToFile: Boolean = false,
    warmupRepititions: Int = 5,
    repititons: Int = 10
) =
  val algorithms: List[MatchingAlgorithm] =
    List(MatchingAlgorithm.StatefulTreeBasedAlgorithm, MatchingAlgorithm.BruteForceAlgorithm)

  val rangeOfRandomMsgs =
    Vector((0 to maxRandomMsgs by rndMsgsStep)*) map { n =>
      (smartHouseMsgs(n), n)
    }

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val m = smartHouseBenchmark(
      smartHouseActions,
      rangeOfRandomMsgs,
      algorithm,
      warmupRepititions,
      repititons
    ).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )

    (algorithm, m)
  }

  if writeToFile then saveToFile("SmartHouse", measurements)
