package join_patterns.examples

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
import org.scalacheck.*

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
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

object GenerateActions:
  // Set seed for the random generator
  Random.setSeed(42)

  private val genMotion: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr
  yield Motion(i, b, s).asInstanceOf[Action]

  private val genAmbientLight: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
    s <- Gen.alphaStr
  yield AmbientLight(i, b, s).asInstanceOf[Action]

  private val genLight: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr
  yield Light(i, b, s).asInstanceOf[Action]

  private val genContact: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr
  yield Contact(i, b, s).asInstanceOf[Action]

  private val genConsumption: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
  yield Consumption(i, b).asInstanceOf[Action]

  private val genHeatingF: Gen[Action] = for
    i <- Gen.choose(0, 100)
    s <- Gen.alphaStr
  yield HeatingF(i, s).asInstanceOf[Action]

  private val genDoorBell: Gen[Action] =
    for i <- Gen.choose(0, 100)
    yield DoorBell(i).asInstanceOf[Action]

  def genActionsOfSizeN(n: Int): Option[List[Action]] =
    val pickAction =
      Gen.oneOf(genMotion, genAmbientLight, genLight, genContact, genConsumption, genHeatingF)
    Gen.containerOfN[List, Action](n, pickAction).sample

def smartHouseExample(algorithm: MatchingAlgorithm, numberOfRandomMsgs: Int) =
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

  val smartHouseActor = Actor[Action, Unit] {
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
          println("Someone entered the bathroom")
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
          println("Someone arrived home")
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
          println("Someone left home")
          Next()

        case ShutOff() =>
          println("Shutting down the smart house. Bye!")
          Stop(())

    }(algorithm)
  }

  val msgs = smartHouseMsgs(numberOfRandomMsgs)

  val (actFut, act) = smartHouseActor.start()
  msgs.foreach(act ! _)

  act ! ShutOff()
  val result = Await.ready(actFut, Duration.Inf)

  result.onComplete(printResult)

def intercalateCorrectMsgs[A](
    correctMsgs: Vector[A],
    randomMsgs: Vector[A]
): Vector[A] =
  val randomMsgsSize  = randomMsgs.size
  val correctMsgsSize = correctMsgs.size
  if randomMsgsSize >= correctMsgsSize then
    val groupSize = (randomMsgsSize + correctMsgsSize - 1) / correctMsgsSize
    randomMsgs
      .grouped(groupSize) // Chunk the random messages into chunks of size groupSize
      .zipAll(correctMsgs, randomMsgs, randomMsgs.headOption.getOrElse(correctMsgs.last))
      .flatMap { case (randomChunk, correctMsg) => randomChunk :+ correctMsg }
      .toVector
  else randomMsgs ++ correctMsgs

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
