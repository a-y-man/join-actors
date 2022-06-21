package test.classes.smartHouse

import java.util.concurrent.LinkedTransferQueue

import join_patterns.{ActorRef, receive}
import test.classes.Msg
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import test.benchmark.Benchmarkable
import java.util.Date
import java.time.Duration
import scala.collection.mutable.{Map => MutMap}

case class Motion(id: Int, status: Boolean, room: String, timestamp: Date) extends Msg
case class Light(id: Int, status: Boolean, room: String)                   extends Msg
case class AmbientLight(id: Int, value: Int, room: String)                 extends Msg
case class Contact(id: Int, open: Boolean, room: String, timestamp: Date)  extends Msg
case class DoorBell(id: Int, debounce: Date)                               extends Msg
case class Consumption(meter_id: Int, value: Int)                          extends Msg
case class HeatingF(id: Int, _type: String)                                extends Msg

class SmartHouse extends Benchmarkable[Msg, Unit] {
  private val q                                 = LinkedTransferQueue[Msg]
  val ref                                       = ActorRef(q)
  var lastNotification                          = Date()
  var electricityConsumption: MutMap[Date, Int] = MutMap()

  val bathroomOccupied =
    (
        mStatus: Boolean,
        lStatus: Boolean,
        mRoom: String,
        lRoom: String,
        alRoom: String,
        value: Int
    ) =>
      mStatus && lStatus && mRoom == "bathroom" && lRoom == "bathroom" && alRoom == "bathroom" && value <= 40

  // the "not" is done logically on the status members
  val turnOff = (mStatus: Boolean, lStatus: Boolean, mRoom: String, lRoom: String, window: Date) =>
    !(mStatus || lStatus) && mRoom == "bathroom" && lRoom == "bathroom" && Duration
      .between(window.toInstant, Date().toInstant)
      .toMinutes < 2

  val sendAlert = (open: Boolean, window: Date) =>
    open && Duration
      .between(window.toInstant, Date().toInstant)
      .toSeconds > 60

  val doorBell = () =>
    Duration
      .between(Date().toInstant, lastNotification.toInstant)
      .toSeconds > 30

  val electicityAlert = (value: Int) =>
    electricityConsumption += (Date(), value)
    electricityConsumption
      .filter((date, _) =>
        Duration
          .between(date.toInstant, Date().toInstant)
          .toDays < 21 // using days, for duration does not natively converts to weeks
      )
      .values
      .sum < 200

  val heatingFailure = (types: Seq[String]) =>
    Duration
      .between(lastNotification.toInstant, Date().toInstant)
      .toSeconds > 60 && types.filter(_ == "floor").size >= 3 && types.exists(_ == "internal")

  val mFrontDoor    = (status: Boolean, room: String) => status && room == "front_door"
  val mEntranceHall = (status: Boolean, room: String) => status && room == "entrance_hall"
  val cFrontDoor    = (status: Boolean, room: String) => status && room == "front_door"

  // add timestamps to check sequential order
  val occupiedHome = (
      mStatus0: Boolean,
      mStatus1: Boolean,
      cStatus: Boolean,
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) => mFrontDoor(mStatus0, mRoom0) && cFrontDoor(cStatus, cRoom) && mEntranceHall(mStatus1, mRoom1)

  // add timestamps to check sequential order
  val emptyHome = (
      mStatus0: Boolean,
      mStatus1: Boolean,
      cStatus: Boolean,
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) => mEntranceHall(mStatus0, mRoom0) && cFrontDoor(cStatus, cRoom) && mFrontDoor(mStatus1, mRoom1)

  protected def f = receive { (y: Msg) =>
    y match
      // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
      case (
            Motion(_: Int, mStatus: Boolean, mRoom: String, _: Date),
            Light(_: Int, lStatus: Boolean, lRoom: String),
            AmbientLight(_: Int, value: Int, alRoom: String)
          ) if bathroomOccupied(mStatus, lStatus, mRoom, lRoom, alRoom, value) =>
        lastNotification = Date()
        println("turn_on_light(l, i, t)")
      // E2. Turn off the lights in a room after two minutes without detecting any movement.
      case (
            Motion(_: Int, mStatus: Boolean, mRoom: String, window: Date),
            Light(_: Int, lStatus: Boolean, lRoom: String)
          ) if turnOff(mStatus, lStatus, mRoom, lRoom, window) =>
        lastNotification = Date()
        println("turn_off_ligth()")
      // E3. Send a notification when a window has been open for over an hour.
      case Contact(_: Int, open: Boolean, _: String, timestamp: Date)
          if sendAlert(open, timestamp) =>
        lastNotification = Date()
        println("send_alert()")
      // E4. Send a notification if someone presses the doorbell, but only if no notification was sent in the past 30 seconds.
      case DoorBell(_: Int, debounce: Date) if doorBell() =>
        lastNotification = Date()
        println("notify()")
      // E5. Detect home arrival or leaving based on a particular sequence of messages, and activate the corresponding scene.
      case (
            Motion(_: Int, mStatus0: Boolean, mRoom0: String, _: Date),
            Contact(_: Int, cStatus: Boolean, cRoom: String, _: Date),
            Motion(_: Int, mStatus1: Boolean, mRoom1: String, _: Date)
          ) if occupiedHome(mStatus0, mStatus1, cStatus, mRoom0, mRoom1, cRoom) =>
        lastNotification = Date()
        println("activate_home_scene(l, i, t)")
      case (
            Motion(_: Int, mStatus0: Boolean, mRoom0: String, _: Date),
            Contact(_: Int, cStatus: Boolean, cRoom: String, _: Date),
            Motion(_: Int, mStatus1: Boolean, mRoom1: String, _: Date)
          ) if emptyHome(mStatus0, mStatus1, cStatus, mRoom0, mRoom1, cRoom) =>
        lastNotification = Date()
        println("activate_empty_scene(l, i, t)")
      // E6. Send a notification if the combined electricity consumption of the past three weeks is greater than 200 kWh.
      case Consumption(_: Int, value: Int) if electicityAlert(value) =>
        lastNotification = Date()
        println("send notification")
      // E7. Send a notification if the boiler fires three Floor Heating Failures and one Internal Failure within the past hour, but only if no notification was sent in the past hour.
      case (
            HeatingF(_: Int, type0: String),
            HeatingF(_: Int, type1: String),
            HeatingF(_: Int, type2: String),
            HeatingF(_: Int, type3: String)
          ) if heatingFailure(List(type0, type1, type2, type3)) =>
        lastNotification = Date()
        println("notify()")
  }

  def run_as_future: concurrent.Future[Long]     = ???
  def run_without_macro: concurrent.Future[Long] = ???

  def run(): Unit = ???
}
