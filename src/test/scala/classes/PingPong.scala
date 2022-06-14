package test.classes.pingPong

import java.util.concurrent.LinkedTransferQueue

import join_patterns.{ActorRef, receive}
import test.classes.Msg
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import test.benchmark.Benchmarkable

case class Ping() extends Msg
case class Pong() extends Msg

class Pinger(private val maxHits: Int) extends Benchmarkable[Pong, Unit] {
  private val q                       = LinkedTransferQueue[Pong]
  var hits                            = 0
  val ref                             = ActorRef[Pong](q)
  var pongRef: Option[ActorRef[Ping]] = None
  var isDone                          = false

  protected def f = receive { (y: Msg) =>
    y match
      case Pong() =>
        hits += 1
        // println(f"ping $hits")
        pongRef.get.send(Ping())

        if hits >= maxHits then isDone = true
    // println("ping is done")
  }

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      ping()
      while !isDone do
        f(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime
      ping()
      while !isDone do
        q.take.asInstanceOf[Pong]
        hits += 1
        pongRef.get.send(Ping())
        if hits >= maxHits then isDone = true

        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    ping()
    while !isDone do
      f(q)
      Thread.`yield`()

  def ping() =
    pongRef.get.send(Ping())
}

class Ponger(private val maxHits: Int) extends Benchmarkable[Ping, Unit] {
  private val q                       = LinkedTransferQueue[Ping]
  var hits                            = 0
  val ref                             = ActorRef(q)
  var pingRef: Option[ActorRef[Pong]] = None
  var isDone                          = false

  protected def f = receive { (y: Msg) =>
    y match
      case Ping() =>
        hits += 1
        // println(f"pong $hits")
        pingRef.get.send(Pong())

        if hits >= maxHits then isDone = true
    // println("pong is done")
  }

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while !isDone do
        f(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while !isDone do
        q.take.asInstanceOf[Ping]
        hits += 1
        pingRef.get.send(Pong())
        if hits >= maxHits then isDone = true

        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while !isDone do
      f(q)
      Thread.`yield`()
}
