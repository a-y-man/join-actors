package benchmark

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
import org.scalacheck.*
import org.scalatest.run
import test.ALGORITHM
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass

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
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield Motion(i, b, s).asInstanceOf[Action]

  private val genAmbientLight: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield AmbientLight(i, b, s).asInstanceOf[Action]

  private val genLight: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield Light(i, b, s).asInstanceOf[Action]

  private val genContact: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield Contact(i, b, s).asInstanceOf[Action]

  private val genConsumption: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
  yield Consumption(i, b).asInstanceOf[Action]

  private val genHeatingF: Gen[Action] = for
    i <- Gen.choose(0, 100)
    s <- Gen.oneOf("internal", "floor")
  yield HeatingF(i, s).asInstanceOf[Action]

  private val genDoorBell: Gen[Action] =
    for i <- Gen.choose(0, 100)
    yield DoorBell(i).asInstanceOf[Action]

  def genActionsOfSizeN(n: Int): Option[List[Action]] =
    val pickAction =
      Gen.oneOf(genMotion, genAmbientLight, genLight, genContact, genConsumption, genHeatingF)
    Gen.containerOfN[List, Action](n, pickAction).sample

def smartHouseExample(algorithm: MatchingAlgorithm) =
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

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
          // println("Someone entered the bathroom")
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
          // println("Someone arrived home")
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
          // println("Someone left home")
          Next()

        case ShutOff() =>
          // println("Shutting down the smart house. Bye!")
          Stop(())

    }(algorithm)
  }

  smartHouseActor

def sendE1(actorRef: ActorRef[Action]) =
  actorRef ! Motion(0, true, "bathroom")
  actorRef ! AmbientLight(0, 30, "bathroom")
  actorRef ! Light(0, false, "bathroom")

def sendE5(actorRef: ActorRef[Action]) =
  if Random.nextInt % 2 == 0 then
    actorRef ! Motion(0, true, "front_door")
    actorRef ! Contact(0, true, "front_door")
    actorRef ! Motion(0, true, "entrance_hall")
  else
    actorRef ! Motion(0, true, "entrance_hall")
    actorRef ! Contact(0, true, "front_door")
    actorRef ! Motion(0, true, "front_door")

def sendE6(actorRef: ActorRef[Action]) =
  actorRef ! Consumption(0, Random.nextInt(100))

def sendE7(ref: ActorRef[Action]) =
  ref ! HeatingF(0, (if Random.nextInt % 4 == 0 then "internal" else "floor"))

def sendE1WithNoise(actorRef: ActorRef[Action], nRndMsgs: Int) =
  val randomMsgs = GenerateActions.genActionsOfSizeN(nRndMsgs).get
  randomMsgs.foreach(actorRef ! _)
  sendE1(actorRef)

def sendE5WithNoise(actorRef: ActorRef[Action], nRndMsgs: Int) =
  val randomMsgs = GenerateActions.genActionsOfSizeN(nRndMsgs).get
  randomMsgs.foreach(actorRef ! _)
  sendE5(actorRef)

def runSmartHouse(smartHouseActions: Int, numberOfRandomMsgs: Int) =
  implicit val ec = ExecutionContext.global

  val actor = smartHouseExample(ALGORITHM)

  Future {
    val startTime = System.nanoTime()

    val (result, actorRef) = actor.start()

    if numberOfRandomMsgs > 0 then
      Random.nextInt(2) match
        case 0 => sendE1WithNoise(actorRef, numberOfRandomMsgs)
        case 1 => sendE5WithNoise(actorRef, numberOfRandomMsgs)

    for i <- 0 to smartHouseActions do
      Random.nextInt(2) match
        case 0 => sendE1(actorRef) // Send messages that trigger E1
        case 1 => sendE5(actorRef) // Send messages that trigger E5

    actorRef ! ShutOff() // Finally, shut off the smart house

    Await.ready(result, Duration(20, "minutes"))

    val endTime = System.nanoTime()
    endTime - startTime
  }

@main
def smartHouseBenchmark =
  val smartHouseActions = 1000
  val randomMsgsPerPass = List(0, 2, 4, 6, 8, 10, 12)

  Benchmark(
    "Smart House",
    10,
    100,
    BenchmarkPass(
      "Control",
      () => runSmartHouse(smartHouseActions, 0)
    ),
    randomMsgsPerPass map { n =>
      BenchmarkPass(
        s"Using ${ALGORITHM.toString} with $n random messages",
        () => runSmartHouse(smartHouseActions, n)
      )
    }
  ).run

// Make sure same input queue for both algos (store sequences or seed)
// Make sure the tree is growing as expected
// Report tree size
