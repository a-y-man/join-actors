package join_patterns

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import java.util.concurrent.LinkedTransferQueue

class PingPongTest extends AnyFunSuite {
  sealed abstract class Msg
  case class Ping() extends Msg
  case class Pong() extends Msg

  class Pinger(private val maxHits: Int) extends Runnable {
    private val q                      = LinkedTransferQueue[Msg]
    var hits                           = 0
    val ref                            = ActorRef(q)
    var pongRef: Option[ActorRef[Msg]] = None
    var isDone                         = false

    private def f = receive { (y: Msg) =>
      y match
        case Pong() =>
          hits += 1
          // println(f"ping $hits")
          pongRef.get.send(Ping())

          if hits >= maxHits then isDone = true
      // println("ping is done")
    }

    override def run =
      ping()
      while !isDone do
        f(q)
        Thread.`yield`()

    def ping() =
      pongRef.get.send(Ping())
  }

  class Ponger(private val maxHits: Int) extends Runnable {
    private val q                      = LinkedTransferQueue[Msg]
    var hits                           = 0
    val ref                            = ActorRef(q)
    var pingRef: Option[ActorRef[Msg]] = None
    var isDone                         = false

    private def f = receive { (y: Msg) =>
      y match
        case Ping() =>
          hits += 1
          // println(f"pong $hits")
          pingRef.get.send(Pong())

          if hits >= maxHits then isDone = true
      // println("pong is done")
    }

    override def run =
      while !isDone do
        f(q)
        Thread.`yield`()
  }

  test("Fixed number of iterations") {
    val maxHits    = 100_000
    val ping       = Pinger(maxHits)
    val pong       = Ponger(maxHits)
    val pingThread = Thread(ping)
    val pongThread = Thread(pong)

    ping.pongRef = Some(pong.ref)
    pong.pingRef = Some(ping.ref)

    println("start")
    pingThread.start
    pongThread.start

    pingThread.join
    pongThread.join
    println("end")

    assert(ping.hits == maxHits)
    assert(pong.hits == maxHits)
  }

  test("Random number of iterations") {
    val maxHits    = Random.nextInt(100_000)
    val ping       = Pinger(maxHits)
    val pong       = Ponger(maxHits)
    val pingThread = Thread(ping)
    val pongThread = Thread(pong)

    ping.pongRef = Some(pong.ref)
    pong.pingRef = Some(ping.ref)

    pingThread.start
    pongThread.start

    pingThread.join
    pongThread.join

    assert(ping.hits == maxHits)
    assert(pong.hits == maxHits)
  }
}
