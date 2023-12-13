package test.scenario.smartHouse

import org.scalatest.funsuite.AnyFunSuite
import test.classes.smartHouse.*

import java.time.Duration
import java.util.Date

class SmartHouseTest extends AnyFunSuite:
  test(
    "E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux."
  ) {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Motion(0, true, "bathroom"))
    house.ref.send(AmbientLight(0, 30, "bathroom"))
    house.ref.send(Light(0, false, "bathroom"))

    houseThread.join
  }

  test("E2. Turn off the lights in a room after two minutes without detecting any movement.") {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Motion(0, false, "bathroom"))
    Thread.sleep(100)
    house.ref.send(Light(0, true, "bathroom"))

    houseThread.join
  }

  test("E3. Send a notification when a window has been open for over an hour.") {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    val twoHoursAgo = Date.from(Date().toInstant.minus(Duration.ofHours(2)))
    house.ref.send(Contact(0, true, "", twoHoursAgo))

    houseThread.join
  }

  test(
    "E4. Send a notification if someone presses the doorbell, but only if no notification was sent in the past 30 seconds."
  ) {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(DoorBell(0))

    houseThread.join
  }

  test(
    "E5. Detect home arrival based on a particular sequence of messages, and activate the corresponding scene."
  ) {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Motion(0, true, "front_door"))
    Thread.sleep(1000)
    house.ref.send(Contact(0, true, "front_door"))
    Thread.sleep(1000)
    house.ref.send(Motion(0, true, "entrance_hall"))

    houseThread.join
  }

  test(
    "E5. Detect home leaving based on a particular sequence of messages, and activate the corresponding scene."
  ) {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Motion(0, true, "entrance_hall"))
    Thread.sleep(1000)
    house.ref.send(Contact(0, true, "front_door"))
    Thread.sleep(1000)
    house.ref.send(Motion(0, true, "front_door"))

    houseThread.join
  }

  test(
    "E6. Send a notification if the combined electricity consumption of the past three weeks is greater than 200 kWh."
  ) {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(Consumption(0, 100))
    house.ref.send(Consumption(0, 110))

    houseThread.join
  }

  test(
    "E7. Send a notification if the boiler fires three Floor Heating Failures and one Internal Failure within the past hour, but only if no notification was sent in the past hour."
  ) {
    val house       = SmartHouse(1)
    val houseThread = Thread(house)

    houseThread.start

    house.ref.send(HeatingF(0, "floor"))
    Thread.sleep(100)
    house.ref.send(HeatingF(0, "floor"))
    Thread.sleep(100)
    house.ref.send(HeatingF(0, "internal"))
    Thread.sleep(100)
    house.ref.send(HeatingF(0, "floor"))

    houseThread.join
  }
