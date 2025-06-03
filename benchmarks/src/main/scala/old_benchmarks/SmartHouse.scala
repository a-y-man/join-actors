package old_benchmarks

import join_actors.api.*
import os.*

import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.*

sealed trait Action
case class Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date()) extends Action
case class AmbientLight(id: Int, lightLevel: Int, room: String, timestamp: Date = Date())
    extends Action
case class Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())   extends Action
case class Contact(id: Int, status: Boolean, room: String, timestamp: Date = Date()) extends Action
case class Consumption(id: Int, consumption: Int, timestamp: Date = Date())          extends Action
case class HeatingF(id: Int, tp: String, timestamp: Date = Date())                   extends Action
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
          Continue
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
          Continue
        case ShutOff() =>
          Stop((System.currentTimeMillis(), actions))
      }
    }(algorithm)
  }

def smartHouseMsgs(n: Int)(generator: Int => Vector[Action]): Vector[Action] =
  val randomMsgs = generator(n)

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
    msgs: Vector[Action],
    algorithm: MatchingAlgorithm
): Future[Measurement] =
  val actor = smartHouseExample(algorithm)

  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    msgs.foreach(actorRef ! _)

    actorRef ! ShutOff()

    val (endTime, matches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }

def smartHouseBenchmark(
    rangeOfRandomMsgs: Vector[(Vector[Action], Int)],
    algorithm: MatchingAlgorithm,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10
) =

  val nullPassMsgs = smartHouseMsgs(5)(GenerateActions.genActionsOfSizeN)

  Benchmark(
    name = "SmartHouse",
    algorithm = algorithm,
    warmupRepetitions = warmupRepetitions,
    repetitions = repetitions,
    nullPass = BenchmarkPass(
      s"Null Pass ${algorithm}",
      () => measureSmartHouse(nullPassMsgs, algorithm)
    ),
    passes = rangeOfRandomMsgs map { case (msgs, n) =>
      BenchmarkPass(
        s"Processing $n number of random messages per match.",
        () => measureSmartHouse(msgs, algorithm)
      )
    }
  )

def runSmartHouseBenchmark(
    smartHouseActions: Int,
    maxRandomMsgs: Int,
    rndMsgsStep: Int,
    writeToFile: Boolean = false,
    warmupRepetitions: Int = 5,
    repetitions: Int = 10,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
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
        Vector.fill(smartHouseActions)(smartHouseMsgs(n)(GenerateActions.genActionsOfSizeN))

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

  val measurements = algorithms map { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    val m = smartHouseBenchmark(
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

  if writeToFile then saveToFile("SmartHouse", measurements, outputDataDir)
