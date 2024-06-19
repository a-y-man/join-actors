package join_patterns.examples.factory_simpl

import actor.*
import actor.Result.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

// Milliseconds in one minute
private val ONE_MIN    = 1000 * 60
private val ONE_DAY    = ONE_MIN * 60 * 24
private val TEN_MIN    = ONE_MIN * 10
private val QUARTER_HR = ONE_MIN * 15
private val THIRTY_MIN = ONE_MIN * 30

enum MachineEvent:
  case Fault(faultID: Int, ts: Long)

enum WorkerEvent:
  case Fix(faultID: Int, ts: Long)

enum SystemEvent:
  case DelayedFault(faultID: Int, ts: Long)
  case Shutdown()

type Event = MachineEvent | WorkerEvent | SystemEvent

import MachineEvent.*, WorkerEvent.*, SystemEvent.*


def monitor(algorithm: MatchingAlgorithm) = 
  Actor[Event, Unit] {
    receive { (self: ActorRef[Event]) =>
      {
        case (Fault(fid1, ts1), Fix(fid2, ts2)) if fid1 == fid2 =>
          println(
            s"========================= ${Console.BLUE}${Console.UNDERLINED}Join Pattern 01${Console.RESET} =========================\n"
          )
          println(
            s"${Console.BLUE}${Console.UNDERLINED}Matched messages: Fault(fid = $fid1, ...), Fix(fid = $fid2, ...)${Console.RESET}\n"
          )
          println(
            s"${Console.GREEN}${Console.UNDERLINED}Fault(fid = $fid1) completed in ${(ts2 - ts1) / ONE_MIN} minutes!${Console.RESET}"
          )
          println(
            s"\n========================= ${Console.BLUE}${Console.UNDERLINED}Join Pattern 01${Console.RESET} ========================="
          )
          Continue

        case (Fault(fid1, ts1), Fault(fid2, ts2), Fix(fid3, ts3))
            if fid2 == fid3 && ts2 > ts1 + TEN_MIN =>
          println(
            s"========================= ${Console.BLUE}${Console.UNDERLINED}Join Pattern 02${Console.RESET} =========================\n"
          )
          println(
            s"${Console.BLUE}${Console.UNDERLINED}Matched messages: Fault(fid = $fid1, ...), Fault(fid = $fid2, ...), Fix(fid = $fid3, ...)${Console.RESET}\n"
          )
          println(
            s"${Console.GREEN}${Console.UNDERLINED}Fault(fid = $fid1, ...) ignored for ${(ts2 - ts1) / ONE_MIN} minutes!${Console.RESET}"
          )
          println(
            s"\n========================= ${Console.BLUE}${Console.UNDERLINED}Join Pattern 02${Console.RESET} ========================="
          )
          self ! DelayedFault(fid1, ts1) // Re-enqueue
          Continue

        case (DelayedFault(fid1, ts1), Fix(fid2, ts2)) if fid1 == fid2 =>
          println(
            s"========================= ${Console.BLUE}${Console.UNDERLINED}Join Pattern 03${Console.RESET} =========================\n"
          )
          println(
            s"${Console.BLUE}${Console.UNDERLINED}Matched messages: DelayedFault(fid = $fid1, ...), Fix(fid = $fid2, ...)${Console.RESET}\n"
          )
          println(
            s"${Console.GREEN}${Console.UNDERLINED}Delayed Fault(fid = $fid1, ...) completed in ${(ts2 - ts1) / ONE_MIN} minutes!${Console.RESET}"
          )
          println(
            s"\n========================= ${Console.BLUE}${Console.UNDERLINED}Join Pattern 03${Console.RESET} ========================="
          )
          Continue

        case Shutdown() => Stop(())
      }
    }(algorithm)
  }

object RunMonitor extends App:
  val events = List(
    // Fault(1, ONE_MIN),
    // Fault(2, ONE_MIN * 5),
    Fault(3, ONE_MIN * 25),
    Fix(3, THIRTY_MIN)
  )

  val algorithm = MatchingAlgorithm.BruteForceAlgorithm

  val (monitorFut, monitorRef) = monitor(algorithm).start()

  events foreach (msg => monitorRef ! msg)

  monitorRef ! Shutdown()

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration

  Await.ready(monitorFut, Duration(15, "m"))
