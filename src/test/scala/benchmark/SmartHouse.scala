package benchmark
import actor.ActorRef
import join_patterns.receive
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
import scala.collection.mutable.{Map => MutMap}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Random

class SmartHouse(private var actions: Int) extends Benchmarkable[Msg, Unit] {
  private var lastNotification     = Date(0L)
  private var lastMotionInBathroom = Date(0L)
  private val isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }
  val bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) => isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

  val occupiedHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "front_door" && cRoom == "front_door" && mRoom1 == "entrance_hall"

  val emptyHome = (
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
}

def sendE1(ref: ActorRef[Msg]) =
  ref.send(Motion(0, true, "bathroom"))
  ref.send(AmbientLight(0, 30, "bathroom"))
  ref.send(Light(0, false, "bathroom"))

def sendE5(ref: ActorRef[Msg]) =
  if Random.nextInt % 2 == 0 then
    ref.send(Motion(0, true, "front_door"))
    ref.send(Contact(0, true, "front_door"))
    ref.send(Motion(0, true, "entrance_hall"))
  else
    ref.send(Motion(0, true, "entrance_hall"))
    ref.send(Contact(0, true, "front_door"))
    ref.send(Motion(0, true, "front_door"))

def sendE6(ref: ActorRef[Msg]) =
  ref.send(Consumption(0, Random.nextInt(100)))

def sendE7(ref: ActorRef[Msg]) =
  ref.send(HeatingF(0, (if Random.nextInt % 4 == 0 then "internal" else "floor")))

@main
def smartHouseBenchmark =
  val smartHouseActions = 6
  Benchmark(
    "Smart House",
    10,
    300,
    BenchmarkPass(
      "Control",
      () => {
        val smartHouse = SmartHouse(smartHouseActions)
        val future     = smartHouse.run_as_future

        for i <- 0 to smartHouseActions do
          Random.nextInt(2) match
            case 0 => sendE1(smartHouse.ref)
            case 1 => sendE5(smartHouse.ref)

        future
      }
    ),
    List(
      BenchmarkPass(
        s"Macro using ${ALGORITHM.toString}",
        () => {
          val smartHouse = SmartHouse(smartHouseActions)
          val future     = smartHouse.run_as_future

          for i <- 0 to smartHouseActions do
            Random.nextInt(2) match
              case 0 => sendE1(smartHouse.ref)
              case 1 => sendE5(smartHouse.ref)

          future
        }
      )
    )
  ).run
