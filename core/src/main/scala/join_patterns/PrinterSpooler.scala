package join_patterns.examples

import actor.*
import actor.Result.*
import join_patterns.MatchingAlgorithm
import join_patterns.MatchingAlgorithm.BruteForceAlgorithm
import join_patterns.MatchingAlgorithm.StatefulTreeBasedAlgorithm
import join_patterns.receive

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Random

enum PrinterSpoolerMessage:
  case Job(jobId: Int, cid: Int, printerId: Int)
  case Ready(printerId: Int)
  case JobDone(jobId: Int)

enum PrinterAuth:
  case Auth(clientID: Int)

type SpoolerMsgs = PrinterSpoolerMessage | PrinterAuth

def printerSpoolerExample(algorithm: MatchingAlgorithm, nPrinters: Int, nJobs: Int): Unit =
  import PrinterSpoolerMessage.*
  import PrinterAuth.*
  val printer: Actor[SpoolerMsgs, Unit] = Actor[SpoolerMsgs, Unit] {
    receive { (self: ActorRef[SpoolerMsgs]) =>
      {
        case (Auth(cid1), Ready(printerId1), Job(jobId, cid2, printerId2))
            if printerId1 == printerId2 && cid1 == cid2 =>
          println(s"send job $jobId to printer $printerId1")
          Continue
        case (Ready(printerId), JobDone(jobId)) =>
          println(s"job $jobId is done by printer $printerId")
          Stop(())
      }
    }(algorithm)
  }

  val (reaction, pRef) = printer.start()

  (1 to nPrinters).foreach { printerId =>
    pRef ! Ready(printerId)
  }

  (1 to nJobs).foreach { jobId =>
    val printerId = Random.nextInt(nPrinters) + 1
    val clientId  = Random.nextInt(10) + 1
    pRef ! Auth(clientId)
    pRef ! Job(jobId, clientId, printerId)
  }

  Await.result(reaction, Duration.Inf)
