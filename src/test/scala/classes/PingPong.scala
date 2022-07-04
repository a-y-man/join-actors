package test.classes.pingPong

import scala.concurrent.{Future, ExecutionContext}

import join_patterns.receive
import actor.ActorRef
import test.classes.Msg
import test.benchmark.Benchmarkable

case class Ping() extends Msg
case class Pong() extends Msg

class Pinger(private val maxHits: Int) extends Benchmarkable[Pong, Unit] {
  var hits                            = 0
  var pongRef: Option[ActorRef[Ping]] = None
  var isDone                          = false

  protected val matcher = receive { (y: Msg) =>
    y match
      case Pong() =>
        // println(f"ping $hits")
        pongRef.get.send(Ping())

        hits += 1
        if hits >= maxHits then isDone = true
    // println("ping is done")
  }

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      ping()
      while !isDone do
        matcher(q)
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
      matcher(q)
      Thread.`yield`()

  def ping() =
    pongRef.get.send(Ping())
}

class Ponger(private val maxHits: Int) extends Benchmarkable[Ping, Unit] {
  var hits                            = 0
  var pingRef: Option[ActorRef[Pong]] = None
  var isDone                          = false

  protected val matcher = receive { (y: Msg) =>
    y match
      case Ping() =>
        // println(f"pong $hits")
        pingRef.get.send(Pong())

        hits += 1
        if hits >= maxHits then isDone = true
    // println("pong is done")
  }

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while !isDone do
        matcher(q)
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
      matcher(q)
      Thread.`yield`()
}
