package test.benchmark.smartHouse

import test.classes.smartHouse.SmartHouse
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}
import actor.ActorRef
import test.classes.smartHouse.{AmbientLight, Consumption, Contact, HeatingF, Light, Motion}
import scala.util.Random

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
  val smartHouseActions = 1_000

  Benchmark(
    "Smart House",
    10,
    300,
    BenchmarkPass(
      "Control",
      () => {
        val smartHouse = SmartHouse(smartHouseActions)
        val future     = smartHouse.run_without_macro

        for i <- 0 to smartHouseActions do
          Random.nextInt(2) match
            case 0 => sendE1(smartHouse.ref)
            case 1 => sendE5(smartHouse.ref)
            /*
            case 2 => sendE6(smartHouse.ref)
            case 3 => sendE7(smartHouse.ref)
             */

        future
      }
    ),
    List(
      BenchmarkPass(
        "Macro",
        () => {
          val smartHouse = SmartHouse(smartHouseActions)
          val future     = smartHouse.run_as_future

          for i <- 0 to smartHouseActions do
            Random.nextInt(2) match
              case 0 => sendE1(smartHouse.ref)
              case 1 => sendE5(smartHouse.ref)
              /*
              case 2 => sendE6(smartHouse.ref)
              case 3 => sendE7(smartHouse.ref)
               */

          future
        }
      )
    )
  ).run
