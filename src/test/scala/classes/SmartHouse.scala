package test.classes.smartHouse

import scala.concurrent.{Future, ExecutionContext}
import java.util.Date
import scala.collection.mutable.{Map => MutMap, ListBuffer}
import java.time.Duration

import join_patterns.receive
import test.classes.Msg
import test.benchmark.Benchmarkable
import actor.Actor
import actor.DynamicActor

case class Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date())  extends Msg
case class Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())   extends Msg
case class AmbientLight(id: Int, value: Int, room: String, timestamp: Date = Date()) extends Msg
case class Contact(id: Int, open: Boolean, room: String, timestamp: Date = Date())   extends Msg
case class DoorBell(id: Int, timestamp: Date = Date())                               extends Msg
case class Consumption(meter_id: Int, value: Int, timestamp: Date = Date())          extends Msg
case class HeatingF(id: Int, _type: String, timestamp: Date = Date())                extends Msg

class SmartHouse(private var actions: Int) extends Benchmarkable[Msg, Unit] {
  private var lastNotification                          = Date(0L)
  private var lastMotionInBathroom                      = Date(0L)
  private var electricityConsumption: MutMap[Date, Int] = MutMap()
  private var failures: MutMap[Date, String]            = MutMap()
  private val isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }
  private val between: (Date, Date) => Duration = (a, b) =>
    Duration.between(a.toInstant, b.toInstant).abs

  val bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) => isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

  val turnOff =
    (times: Seq[Date], rooms: Seq[String], mStatus: Boolean, lStatus: Boolean, window: Duration) =>
      isSorted(times) && rooms.forall(_ == "bathroom") && !mStatus && lStatus && between(
        lastMotionInBathroom,
        Date()
      ).compareTo(window) > 0

  val sendAlert = (open: Boolean, timestamp: Date, window: Duration) =>
    open && between(timestamp, Date()).compareTo(window) > 0

  val doorBell = (debounce: Duration) => between(lastNotification, Date()).compareTo(debounce) > 0

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

  val electicityAlert = (window: Duration, threshold: Int) =>
    electricityConsumption
      .filter((date, _) => between(date, Date()).compareTo(window) > 0)
      .values
      .sum < threshold

  val heatingFailure = (window: Duration) =>
    val pastHourFailures = failures
      .filter((date, _) => between(date, Date()).compareTo(window) < 0)

    between(lastNotification, Date()).compareTo(window) < 0 && pastHourFailures.values
      .filter(_ == "floor")
      .size >= 3 && pastHourFailures.values.exists(_ == "internal")

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
      // E2. Turn off the lights in a room after two minutes without detecting any movement.
      case (
            Motion(_: Int, mStatus: Boolean, mRoom: String, t0: Date),
            Light(_: Int, lStatus: Boolean, lRoom: String, t1: Date)
          ) if turnOff(List(t0, t1), List(mRoom, lRoom), mStatus, lStatus, Duration.ofMinutes(2)) =>
        lastNotification = Date()
        lastMotionInBathroom = lastNotification
        // println("turn_off_light()")
        actions -= 1
      // E3. Send a notification when a window has been open for over an hour.
      case Contact(_: Int, open: Boolean, _: String, timestamp: Date)
          if sendAlert(open, timestamp, Duration.ofHours(1)) =>
        lastNotification = Date()
        // println("send_alert()")
        actions -= 1
      // E4. Send a notification if someone presses the doorbell, but only if no notification was sent in the past 30 seconds.
      case DoorBell(_: Int, _: Date) if doorBell(Duration.ofSeconds(30)) =>
        lastNotification = Date()
        // println("notify()")
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
      // E6. Send a notification if the combined electricity consumption of the past three weeks is greater than 200 kWh.
      case Consumption(_: Int, value: Int, timestamp: Date) =>
        electricityConsumption.addOne((timestamp, value))
        lastNotification = Date()

        if electicityAlert(
            Duration.ofDays(21) /* using days, for duration does not natively converts to weeks */,
            200
          )
        then
          // println("send notification")
          actions -= 1
      // E7. Send a notification if the boiler fires three Floor Heating Failures and one Internal Failure within the past hour, but only if no notification was sent in the past hour.
      case HeatingF(_: Int, _type: String, timestamp: Date) =>
        failures += (timestamp, _type)
        lastNotification = Date()

        if heatingFailure(Duration.ofHours(1)) then
          // println("notify()")
          actions -= 1
  }

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    import collection.convert.ImplicitConversions._
    implicit val ec = ExecutionContext.global

    Future {
      val start                     = System.nanoTime
      val messages: ListBuffer[Msg] = ListBuffer()

      while actions > 0 do
        val light        = messages.find(_.isInstanceOf[Light])
        val motions      = messages.filter(_.isInstanceOf[Motion])
        val ambientLight = messages.find(_.isInstanceOf[AmbientLight])

        if light.isDefined && !motions.isEmpty && ambientLight.isDefined && bathroomOccupied(
            List(
              motions(0).asInstanceOf[Motion].timestamp,
              ambientLight.get.asInstanceOf[AmbientLight].timestamp,
              light.get.asInstanceOf[Light].timestamp
            ),
            List(
              motions(0).asInstanceOf[Motion].room,
              light.get.asInstanceOf[Light].room,
              ambientLight.get.asInstanceOf[AmbientLight].room
            ),
            motions(0).asInstanceOf[Motion].status,
            light.get.asInstanceOf[Light].status,
            ambientLight.get.asInstanceOf[AmbientLight].value
          )
        then
          lastNotification = Date()
          lastMotionInBathroom = lastNotification
          // println("turn_on_light(l, i, t)")
          actions -= 1
        else if light.isDefined && !motions.isEmpty && turnOff(
            List(
              motions(0).asInstanceOf[Motion].timestamp,
              light.get.asInstanceOf[Light].timestamp
            ),
            List(motions(0).asInstanceOf[Motion].room, light.get.asInstanceOf[Light].room),
            motions(0).asInstanceOf[Motion].status,
            light.get.asInstanceOf[Light].status,
            Duration.ofMinutes(2)
          )
        then
          lastNotification = Date()
          lastMotionInBathroom = lastNotification
          // println("turn_off_light()")
          actions -= 1
        else
          val contact = messages.find(_.isInstanceOf[Contact])

          if contact.isDefined && sendAlert(
              contact.get.asInstanceOf[Contact].open,
              contact.get.asInstanceOf[Contact].timestamp,
              Duration.ofHours(1)
            )
          then
            lastNotification = Date()
            // println("send_alert()")
            actions -= 1
          else if messages.find(_.isInstanceOf[DoorBell]).isDefined && doorBell(
              Duration.ofSeconds(30)
            )
          then
            lastNotification = Date()
            // println("notify()")
            actions -= 1
          else if motions.size >= 2 && contact.isDefined then
            if occupiedHome(
                List(
                  motions(0).asInstanceOf[Motion].timestamp,
                  contact.get.asInstanceOf[Contact].timestamp,
                  motions(1).asInstanceOf[Motion].timestamp
                ),
                List(
                  motions(0).asInstanceOf[Motion].status,
                  motions(1).asInstanceOf[Motion].status,
                  contact.get.asInstanceOf[Contact].open
                ),
                motions(0).asInstanceOf[Motion].room,
                motions(1).asInstanceOf[Motion].room,
                contact.get.asInstanceOf[Contact].room
              )
            then lastNotification = Date()
            // println("activate_home_scene(l, i, t)")
            actions -= 1
          else if motions.size >= 2 && emptyHome(
              List(
                motions(0).asInstanceOf[Motion].timestamp,
                contact.get.asInstanceOf[Contact].timestamp,
                motions(1).asInstanceOf[Motion].timestamp
              ),
              List(
                motions(0).asInstanceOf[Motion].status,
                motions(1).asInstanceOf[Motion].status,
                contact.get.asInstanceOf[Contact].open
              ),
              motions(0).asInstanceOf[Motion].room,
              motions(1).asInstanceOf[Motion].room,
              contact.get.asInstanceOf[Contact].room
            )
          then
            lastNotification = Date()
            // println("activate_empty_scene(l, i, t)")
            actions -= 1
          else
            val consumption = messages.find(_.isInstanceOf[Consumption])

            if consumption.isDefined then
              val _consumption = consumption.get.asInstanceOf[Consumption]

              electricityConsumption.addOne((_consumption.timestamp, _consumption.value))
              lastNotification = Date()

              if electicityAlert(Duration.ofDays(21), 200) then
                // println("send notification")
                actions -= 1
            else
              val heatingF = messages.find(_.isInstanceOf[HeatingF])

              if heatingF.isDefined then
                val _heatingF = consumption.get.asInstanceOf[HeatingF]

                failures += (_heatingF.timestamp, _heatingF._type)
                lastNotification = Date()

                if heatingFailure(Duration.ofHours(1)) then
                  // println("notify()")
                  actions -= 1
              else
                messages.append(q.take())
                q.drainTo(messages)

        Thread.`yield`()

      System.nanoTime - start
    }

  def run(): Unit =
    while actions > 0 do
      matcher(q)
      Thread.`yield`()
}

package smallSmartHouse {
  case class Motion(status: Boolean, room: String) extends Msg
  case class Light(status: Boolean, room: String)  extends Msg
  case class Contact(open: Boolean, room: String)  extends Msg

  class SmallSmartHouse(private var actions: Int) extends Actor[Msg, Unit] {
    private var lastMotionInBathroom = Date(0L)

    def turnOff(rooms: Seq[String], mStatus: Boolean, lStatus: Boolean, window: Duration) = ???

    def occupiedHome(
        statuses: Seq[Boolean],
        mRoom0: String,
        mRoom1: String,
        cRoom: String
    ): Boolean = ???

    protected val matcher = receive { (y: Msg) =>
      y match
        // E2. Turn off the lights in a room after two minutes without detecting any movement.
        case (
              Motion(mStatus: Boolean, mRoom: String),
              Light(lStatus: Boolean, lRoom: String)
            ) if turnOff(List(mRoom, lRoom), mStatus, lStatus, Duration.ofMinutes(2)) =>
          lastMotionInBathroom = Date()
          println("turn_off_light()")
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
          println("activate_home_scene(l, i, t)")
    }

    def run(): Unit = ???
  }

  class DynSmallSmartHouse(private var actions: Int) extends DynamicActor[Msg, Unit] {
    private var lastMotionInBathroom                      = Date(0L)
    private var electricityConsumption: MutMap[Date, Int] = MutMap()
    private var failures: MutMap[Date, String]            = MutMap()

    def turnOff(rooms: Seq[String], mStatus: Boolean, lStatus: Boolean, window: Duration) = ???

    def occupiedHome(
        statuses: Seq[Boolean],
        mRoom0: String,
        mRoom1: String,
        cRoom: String
    ): Boolean = ???

    def electicityAlert(window: Duration, threshold: Int) = ???

    def heatingFailure(window: Duration) = ???

    protected var matcher = receive { (y: Msg) =>
      y match
        // E2. Turn off the lights in a room after two minutes without detecting any movement.
        case (
              Motion(mStatus: Boolean, mRoom: String),
              Light(lStatus: Boolean, lRoom: String)
            ) if turnOff(List(mRoom, lRoom), mStatus, lStatus, Duration.ofMinutes(2)) =>
          lastMotionInBathroom = Date()
          println("turn_off_light()")
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
          println("activate_home_scene(l, i, t)")
    }

    def changeConfiguration =
      matcher = receive { (y: Msg) =>
        y match
          // E6. Send a notification if the combined electricity consumption of the past three weeks is greater than 200 kWh.
          case Consumption(_: Int, value: Int, timestamp: Date) =>
            electricityConsumption.addOne((timestamp, value))

            if electicityAlert(
                Duration.ofDays(
                  21
                ) /* using days, for duration does not natively converts to weeks */,
                200
              )
            then println("send notification")
          // E7. Send a notification if the boiler fires three Floor Heating Failures and one Internal Failure within the past hour, but only if no notification was sent in the past hour.
          case HeatingF(_: Int, _type: String, timestamp: Date) =>
            failures += (timestamp, _type)

            if heatingFailure(Duration.ofHours(1)) then println("notify()")
      }

    def run(): Unit = ???
  }
}
