package factory_simpl

import actor.*
import join_patterns.receive_

// Milliseconds in one minute
private val ONE_MIN    = 1000 * 60
private val ONE_DAY    = ONE_MIN * 60 * 24
private val TEN_MIN    = ONE_MIN * 10
private val THIRTY_MIN = ONE_MIN * 30

enum MachineEvent:
  case MaintenanceRequest(machineId: Int, reqId: Int, reason: String, ts: Long)
  // ...

enum WorkerEvent:
  case RequestTaken(workerId: Int, reqId: Int, ts: Long)
  // ...

enum SystemEvent:
  case DelayedMaintenanceRequest(machineId: Int, reqId: Int, reason: String, ts: Long)
  // ...
  case Shutdown()

type Event = MachineEvent | WorkerEvent | SystemEvent

def monitor() = Actor[Event, Unit] {
  import MachineEvent.*, WorkerEvent.*, SystemEvent.*

  receive_ { (self: ActorRef[Event]) =>
    {
      case (MaintenanceRequest(_, rid1, _, ts1), RequestTaken(_, rid2, ts2)) if rid1 == rid2 =>
        // updateMaintenanceStats(ts1, ts2)
        Next()

      case (
            MaintenanceRequest(mid, rid1, reason, ts1),
            MaintenanceRequest(_, rid2, _, ts2),
            RequestTaken(wid, rid3, ts3)
          )
          if rid2 == rid3
            && ts2 > ts1 + TEN_MIN =>
        // updateMaintenanceStats(ts2, ts3)
        println(s"Request ${rid1} ignored for ${(ts2 - ts1) / ONE_MIN} minutes!")
        self ! DelayedMaintenanceRequest(mid, rid1, reason, ts1) // Re-enqueue
        Next()

      case (DelayedMaintenanceRequest(_, rid1, _, ts1), RequestTaken(_, rid2, ts2))
          if rid1 == rid2 =>
        // updateMaintenanceStats(ts1, ts2)
        Next()

      // ... more cases omitted ...

      case Shutdown() =>
        Stop(())
    }
  }(join_patterns.MatchingAlgorithm.StatefulTreeBasedAlgorithm)
}
