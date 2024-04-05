package factory

import actor.*
import join_patterns.receive_

// Milliseconds in one minute
private val ONE_MIN    = 1000 * 60
private val ONE_DAY    = ONE_MIN * 60 * 24
private val TEN_MIN    = ONE_MIN * 10
private val THIRTY_MIN = ONE_MIN * 30

enum MachineEvent:
  case MaintenanceRequest(machineId: Int, reqId: Int, ts: Long)

enum WorkerEvent:
  case RequestTaken(workerId: Int, reqId: Int, ts: Long)
  case RequestCompleted(workerId: Int, reqId: Int, ts: Long)
  case RequestDropped(workerId: Int, reqId: Int, ts: Long)
  case RequestResumed(workerId: Int, reqId: Int, ts: Long)

enum SystemEvent:
  case Mark(ts: Long, used: Set[Int]) // Mark 'used' on request ids
  case Shutdown()

type Event = MachineEvent | WorkerEvent | SystemEvent

def monitor() = Actor[Event, Unit] {
  import MachineEvent.*, WorkerEvent.*, SystemEvent.*

  receive_ { (self: ActorRef[Event]) =>
    // A machine sends a maintenance request, a worker takes it, fixes it, but
    // the same machine sends a new maintenance request in less than 30 min
    {
      case (
            MaintenanceRequest(mid1, rid1, ts1),
            RequestCompleted(_, rid2, _),
            MaintenanceRequest(mid3, rid3, ts3)
          )
          if rid1 == rid2
            && mid1 == mid3
            && (ts3 - ts1).abs < THIRTY_MIN =>
        self ! MaintenanceRequest(mid3, rid3, ts3) // Re-enqueue latest request
        println(s"Machine ${mid1} broke within 30 minutes after maintenance!")
        Next()

      // A machine sends a maintenance request that is only taken after a certain
      // maximum time (10 minutes)
      case (MaintenanceRequest(mid1, rid1, ts1), RequestTaken(_, rid2, ts2))
          if rid1 == rid2
            && ts2 - ts1 >= TEN_MIN =>
        println(s"Request ${rid1} only taken after ${(ts2 - ts1) / ONE_MIN} minutes!")
        self ! MaintenanceRequest(mid1, rid1, ts1) // Re-enqueue latest request
        Next()
      case (MaintenanceRequest(mid1, rid1, ts1), Mark(ts2, used))
          if ts2 - ts1 >= TEN_MIN
            && !used.contains(rid1) =>
        self ! MaintenanceRequest(mid1, rid1, ts1) // Re-enqueue latest request
        self ! Mark(ts2, used + mid1)              // Re-enqueue "used" mark for this request
        println(s"Request ${rid1} not taken for ${(ts2 - ts1) / ONE_MIN} minutes!")
        Next()

      // A maintenance request was taken within ten minutes: all good
      case (MaintenanceRequest(mid1, rid1, ts1), RequestTaken(_, rid2, ts2))
          if rid1 == rid2
            && (ts2 - ts1).abs < TEN_MIN =>
        self ! MaintenanceRequest(mid1, rid1, ts1) // Re-enqueue latest request
        Next()                                     // Nothing to report

      // Get rid of old completed maintenance requests
      case (MaintenanceRequest(mid1, rid1, ts1), RequestCompleted(_, rid2, _), Mark(ts3, used3))
          if rid1 == rid2
            && ts3 - ts1 > THIRTY_MIN =>
        // The event are now useless, we only re-enqueue the mark
        self ! Mark(ts3, used3)
        Next()

      // Get rid of old marks
      case (Mark(ts1, _), Mark(ts2, used2)) if ts2 - ts1 >= 2 * TEN_MIN =>
        self ! Mark(ts2, used2) // Re-enqueue newest mark only
        Next()

      // A machine sends 3 maintenance requests within 24 hours. NOTE: this case
      // would (quite likely) never trigger: the cases above would consume some of
      // the maintenance requests, so this case would only trigger if all requests
      // are unhandled. Maybe we should copy the incoming stream of requests to a
      // separate actor that handles this pattern
      case (
            MaintenanceRequest(mid1, _, ts1),
            MaintenanceRequest(mid2, _, ts2),
            MaintenanceRequest(mid3, _, ts3)
          )
          if mid1 == mid2 && mid2 == mid3
            && ts1 <= ts2 && ts2 <= ts3
            && ts3 - ts1 < ONE_DAY =>
        println(s"Machine ${mid1} broke 3 times in 24 hours!")
        Next()

      case Shutdown() =>
        Stop(())
    }
  }(join_patterns.MatchingAlgorithm.StatefulTreeBasedAlgorithm)
}
