package benchmark
import actor.ActorRef
import join_patterns.receive
import org.scalacheck.Gen
import test.ALGORITHM
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass
import test.benchmark.Benchmarkable
import test.classes.Msg
import test.classes.smartHouse.AmbientLight
import test.classes.smartHouse.Consumption
import test.classes.smartHouse.Contact
import test.classes.smartHouse.HeatingF
import test.classes.smartHouse.Light
import test.classes.smartHouse.Motion
import test.classes.smartHouse.SmartHouse

import java.time.Duration
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map as MutMap}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Random

class SmartHouse(private var actions: Int) extends Benchmarkable[Msg, Unit]:
  private var lastNotification     = Date(0L)
  private var lastMotionInBathroom = Date(0L)
  private def isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }
  private def bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) => isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

  private def occupiedHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "front_door" && cRoom == "front_door" && mRoom1 == "entrance_hall"

  private def emptyHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "entrance_hall" && cRoom == "front_door" && mRoom1 == "front_door"

  protected val matcher = receive { (y: Msg) =>
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
        // println("turn_on_light(l, i, t)")
        actions -= 1
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
        // println("activate_home_scene(l, i, t)")
        actions -= 1
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
        // println("activate_empty_scene(l, i, t)")
        actions -= 1
  }(ALGORITHM)

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run(): Unit =
    while actions > 0 do
      matcher(q)
      Thread.`yield`()

object GenerateTestMsgs:
  // Set seed for the random generator
  Random.setSeed(42)

  private val genMotion: Gen[Msg] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield Motion(i, b, s).asInstanceOf[Msg]

  private val genAmbientLight: Gen[Msg] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield AmbientLight(i, b, s).asInstanceOf[Msg]

  private val genLight: Gen[Msg] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield Light(i, b, s).asInstanceOf[Msg]

  private val genContact: Gen[Msg] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.oneOf("bathroom", "front_door", "entrance_hall")
  yield Contact(i, b, s).asInstanceOf[Msg]

  private val genConsumption: Gen[Msg] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
  yield Consumption(i, b).asInstanceOf[Msg]

  private val genHeatingF: Gen[Msg] = for
    i <- Gen.choose(0, 100)
    s <- Gen.oneOf("internal", "floor")
  yield HeatingF(i, s).asInstanceOf[Msg]

  def genMsgsOfSizeN(n: Int): Option[List[Msg]] =
    val pickMsg =
      Gen.oneOf(genMotion, genAmbientLight, genLight, genContact, genConsumption, genHeatingF)
    Gen.containerOfN[List, Msg](n, pickMsg).sample

def sendE1(actorRef: ActorRef[Msg]) =
  actorRef ! Motion(0, true, "bathroom")
  actorRef ! AmbientLight(0, 30, "bathroom")
  actorRef ! Light(0, false, "bathroom")

def sendE5(actorRef: ActorRef[Msg]) =
  if Random.nextInt % 2 == 0 then
    actorRef ! Motion(0, true, "front_door")
    actorRef ! Contact(0, true, "front_door")
    actorRef ! Motion(0, true, "entrance_hall")
  else
    actorRef ! Motion(0, true, "entrance_hall")
    actorRef ! Contact(0, true, "front_door")
    actorRef ! Motion(0, true, "front_door")

def sendE6(actorRef: ActorRef[Msg]) =
  actorRef ! Consumption(0, Random.nextInt(100))

def sendE7(ref: ActorRef[Msg]) =
  ref ! HeatingF(0, (if Random.nextInt % 4 == 0 then "internal" else "floor"))

def sendE1WithNoise(actorRef: ActorRef[Msg], nRndMsgs: Int) =
  val randomMsgs = GenerateTestMsgs.genMsgsOfSizeN(nRndMsgs).get
  randomMsgs.foreach(actorRef ! _)
  sendE1(actorRef)

def sendE5WithNoise(actorRef: ActorRef[Msg], nRndMsgs: Int) =
  val randomMsgs = GenerateTestMsgs.genMsgsOfSizeN(nRndMsgs).get
  randomMsgs.foreach(actorRef ! _)
  sendE5(actorRef)

def smartHouseBenchmark =
  val smartHouseActions = 1000
  Benchmark(
    "Smart House",
    10,
    100,
    BenchmarkPass(
      "Control",
      () =>
        val smartHouse = SmartHouse(smartHouseActions)
        val future     = smartHouse.run_as_future

        for i <- 0 to smartHouseActions do
          Random.nextInt(2) match
            case 0 => sendE1(smartHouse.ref)
            case 1 => sendE5(smartHouse.ref)

        future
    ),
    List(
      BenchmarkPass(
        s"Using ${ALGORITHM.toString}",
        () =>
          val smartHouse = SmartHouse(smartHouseActions)
          val future     = smartHouse.run_as_future

          for i <- 0 to smartHouseActions do
            Random.nextInt(2) match
              case 0 => sendE1(smartHouse.ref)
              case 1 => sendE5(smartHouse.ref)

          future
      )
    )
  ).run

def smartHouseBenchmarkWithNoise =
  val smartHouseActions = 15

  val numberOfRndMsgs = Range(0, 22, 2)

  val benchmarkPasses = numberOfRndMsgs.map { n =>
    BenchmarkPass(
      s"Using ${ALGORITHM.toString} with $n random messages",
      () =>
        val smartHouse = SmartHouse(smartHouseActions)
        val future     = smartHouse.run_as_future

        for i <- 0 to smartHouseActions do
          Random.nextInt(2) match
            case 0 => sendE1WithNoise(smartHouse.ref, n)
            case 1 => sendE5WithNoise(smartHouse.ref, n)

        future
    )
  }

  Benchmark(
    "Smart House benchmark with noise (random messages)",
    5,
    20,
    BenchmarkPass(
      "Control",
      () =>
        val smartHouse = SmartHouse(smartHouseActions)
        val future     = smartHouse.run_as_future

        for i <- 0 to smartHouseActions do
          Random.nextInt(2) match
            case 0 => sendE1WithNoise(smartHouse.ref, 0)
            case 1 => sendE5WithNoise(smartHouse.ref, 0)

        future
    ),
    benchmarkPasses
  ).run

@main
def runBenchmark =
  // println("Running benchmark")
  smartHouseBenchmark
  // println("Running benchmark with noise")
  // smartHouseBenchmarkWithNoise
