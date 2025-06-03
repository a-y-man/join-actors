package new_benchmarks.simple_smart_house

import java.util.Date

sealed trait Action
case class Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date()) extends Action
case class AmbientLight(id: Int, lightLevel: Int, room: String, timestamp: Date = Date())
  extends Action
case class Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())   extends Action
case class Contact(id: Int, status: Boolean, room: String, timestamp: Date = Date()) extends Action
case class Consumption(id: Int, consumption: Int, timestamp: Date = Date())          extends Action
case class HeatingF(id: Int, tp: String, timestamp: Date = Date())                   extends Action
case class DoorBell(id: Int, timestamp: Date = Date())                               extends Action
case class ShutOff()                                                                 extends Action
