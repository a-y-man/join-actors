package join_patterns

import org.scalatest.funsuite._
import java.util.concurrent.LinkedTransferQueue

class PingPongTest extends AnyFunSuite {
  sealed abstract class Msg
  case class Ping() extends Msg
  case class Pong() extends Msg

  class _Ping(private val maxHits: Int) {
    private val q = LinkedTransferQueue[Msg]
    var hits = 0
    val ref = ActorRef(q)
    var pongRef: Option[ActorRef[Msg]] = None
    var isDone = false

    private def f = receive { (y: Msg) => y match
      case Pong() =>
        hits += 1
        //println(f"ping $hits")
        pongRef.get.send(Ping())

        if hits >= maxHits then
          isDone = true
          //println("ping is done")
    }

    def apply() = f(q)

    def ping() =
      pongRef.get.send(Ping())
  }

  class _Pong(private val maxHits: Int) {
    private val q = LinkedTransferQueue[Msg]
    var hits = 0
    val ref = ActorRef(q)
    var pingRef: Option[ActorRef[Msg]] = None
    var isDone = false

    private def f = receive { (y: Msg) => y match
      case Ping() =>
        hits += 1
        //println(f"pong $hits")
        pingRef.get.send(Pong())

        if hits >= maxHits then
          isDone = true
          //println("pong is done")
    }

    def apply() = f(q)
  }

  test("Fixed number of iterations") {
    val maxHits = 100_000
    val ping = _Ping(maxHits)
    val pong = _Pong(maxHits)

    ping.pongRef = Some(pong.ref)
    pong.pingRef = Some(ping.ref)

    val pingThread = new Thread {
      override def run =
        ping.ping()

        while !ping.isDone do ping()
    }

    val pongThread = new Thread {
      override def run = while !pong.isDone do pong()
    }

    println("start")
    pingThread.start
    pongThread.start

    pingThread.join
    pongThread.join
    println("end")

    assert(ping.hits == maxHits)
    assert(pong.hits == maxHits)
  }
}