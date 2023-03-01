package test.scenario.demo

import java.util.Date
import java.time.Duration
import org.scalatest.funsuite.AnyFunSuite

import join_patterns.receive
import test.classes.Msg
import test.benchmark.Benchmarkable
import actor.Actor
import join_patterns.AlgorithmType
import test.ALGORITHM

case class Motion(status: Boolean, room: String) extends Msg
case class Light(status: Boolean, room: String)  extends Msg
case class Contact(open: Boolean, room: String)  extends Msg

class DemoSmartHouse() extends Actor[Msg, Unit] {
  private var lastMotionInBathroom = Date(0L)
  private var lastNotification     = Date(0L)
  private val between: (Date, Date) => Duration = (a, b) =>
    Duration.between(a.toInstant, b.toInstant).abs

  def turnOff(rooms: Seq[String], mStatus: Boolean, lStatus: Boolean, window: Duration) =
    rooms.forall(_ == "bathroom") && !mStatus && lStatus && between(
      lastMotionInBathroom,
      Date()
    ).compareTo(window) > 0

  def occupiedHome(
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ): Boolean =
    statuses.forall(
      _ == true
    ) && mRoom0 == "front_door" && cRoom == "front_door" && mRoom1 == "entrance_hall"

  protected val matcher = receive { (y: Msg) =>
    y match
      // E2. Turn off the lights in a room after two minutes without detecting any movement.
      case (
            Motion(mStatus: Boolean, mRoom: String),
            Light(lStatus: Boolean, lRoom: String)
          ) if turnOff(List(mRoom, lRoom), mStatus, lStatus, Duration.ofMinutes(2)) =>
        lastMotionInBathroom = Date()
        // println("turn_off_light()")
      // E5. Detect home arrival based on a particular sequence of messages, and activate the corresponding scene.
      case (
            Motion(mStatus0: Boolean, mRoom0: String),
            Contact(cStatus: Boolean, cRoom: String),
            Motion(mStatus1: Boolean, mRoom1: String)
          )
          if occupiedHome(
            List(mStatus0, mStatus1, cStatus),
            mRoom0,
            mRoom1,
            cRoom
          ) =>
        lastNotification = Date()
        // println("activate_home_scene(l, i, t)")

  }(ALGORITHM)

  def run(): Unit =
    matcher(q)
    Thread.`yield`()
}

class SmartHouseTest extends AnyFunSuite {
  // val algorithm: AlgorithmType = AlgorithmType.BasicAlgorithm
  test("E2. Turn off the lights in a room after two minutes without detecting any movement.") {
    val house       = DemoSmartHouse()
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Motion(false, "bathroom"))
    house.ref.send(Light(true, "bathroom"))

    houseThread.join
  }

  test(
    "E5. Detect home arrival based on a particular sequence of messages, and activate the corresponding scene."
  ) {
    val house       = DemoSmartHouse()
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Motion(true, "front_door"))
    house.ref.send(Contact(true, "front_door"))
    house.ref.send(Motion(true, "entrance_hall"))

    houseThread.join
  }
}
