package join_actors.examples

import join_actors.api.*
import org.scalacheck.*

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.*

/* This is a definition of the messages accepted by the Smart house actor */
enum Action:
  case Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date())
  case AmbientLight(id: Int, value: Int, room: String, timestamp: Date = Date())
  case Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())
  case Contact(id: Int, open: Boolean, room: String, timestamp: Date = Date())
  case Consumption(meter_id: Int, value: Int, timestamp: Date = Date())
  case HeatingF(id: Int, _type: String, timestamp: Date = Date())
  case DoorBell(id: Int, timestamp: Date = Date())
  case ShutOff()

import Action.*

/*
This function defines a smart house example using join patterns.
 */
def smartHouseExample(algorithm: MatchingAlgorithm) =
  var lastNotification     = Date(0L)
  var lastMotionInBathroom = Date(0L)
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

  Actor[Action, Unit] {
    receive { (selfRef: ActorRef[Action]) =>
      { // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
        case Motion(_: Int, mStatus: Boolean, mRoom: String, t0: Date)
              &:& AmbientLight(_: Int, value: Int, alRoom: String, t1: Date)
              &:& Light(_: Int, lStatus: Boolean, lRoom: String, t2: Date)
            if bathroomOccupied(
              List(t0, t1, t2),
              List(mRoom, lRoom, alRoom),
              mStatus,
              lStatus,
              value
            ) =>
          lastNotification = Date()
          lastMotionInBathroom = lastNotification
          println("Someone entered the bathroom")
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
          println("Someone arrived home")
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
          println("Someone left home")
          Continue

        case ShutOff() =>
          println("Shutting down the smart house. Bye!")
          Stop(())
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

// val msgs = smartHouseMsgs(numberOfRandomMsgs)(GenerateActions.genActionsOfSizeN)
def runSmartHouseExample(
    algorithm: MatchingAlgorithm,
    msgs: Vector[Action]
) =
  val smartHouseActor = smartHouseExample(algorithm)
  val (actFut, act)   = smartHouseActor.start()
  val startTime       = System.currentTimeMillis()
  msgs.foreach(act ! _)
  act ! ShutOff()
  val result = Await.ready(actFut, Duration.Inf)

  result.onComplete(printResult)
