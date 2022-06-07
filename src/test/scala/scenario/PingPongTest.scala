package test.scenario.pingPong

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

import test.classes.pingPong._

class PingPongTest extends AnyFunSuite {
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
