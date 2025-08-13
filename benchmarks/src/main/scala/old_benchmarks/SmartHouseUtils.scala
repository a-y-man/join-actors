package old_benchmarks.smart_house_utils

import java.util.Date

enum Action:
  case Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date())
  case AmbientLight(id: Int, lightLevel: Int, room: String, timestamp: Date = Date())
  case Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())
  case Contact(id: Int, status: Boolean, room: String, timestamp: Date = Date())
  case Consumption(id: Int, consumption: Int, timestamp: Date = Date())
  case HeatingF(id: Int, tp: String, timestamp: Date = Date())
  case DoorBell(id: Int, timestamp: Date = Date())
  case ShutOff()




