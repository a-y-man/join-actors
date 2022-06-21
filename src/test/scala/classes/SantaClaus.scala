package test.classes.santaClaus

import java.util.concurrent.LinkedTransferQueue

import join_patterns.{ActorRef, receive}
import test.classes.Msg
import test.benchmark.Benchmarkable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.collection.mutable.ListBuffer

case class IsBack(n: Int)   extends Msg
case class CanLeave()       extends Msg
case class Helped()         extends Msg
case class NeedHelp(n: Int) extends Msg

class SantaClaus(val reindeerNumber: Int, val elvesNumber: Int, var actions: Int)
    extends Benchmarkable[Msg, Unit] {
  private val q                     = LinkedTransferQueue[Msg]
  val ref                           = ActorRef(q)
  val reinDeerRefs                  = Array.fill[Option[ActorRef[CanLeave]]](reindeerNumber)(None)
  val elvesRefs                     = Array.fill[Option[ActorRef[Helped]]](elvesNumber)(None)
  private var reindeersBack         = Array.fill[Boolean](reindeerNumber)(false)
  private val _println: Any => Unit = (x: Any) => println(f"${this.getClass.getSimpleName}: $x")

  private val f = receive { (y: Msg) =>
    y match
      case IsBack(n: Int) =>
        // reindeersBack.foreach(r => println("\t" + r))
        reindeersBack(n) = true

        if reindeersBack.forall(r => r) then
          _println("awake")
          _println("delivering presents")
          reinDeerRefs.foreach(_.get.send(CanLeave()))
          reindeersBack = Array.fill[Boolean](reindeerNumber)(false)
          _println("sleeping")
          actions -= 1
      case (NeedHelp(n0: Int), NeedHelp(n1: Int), NeedHelp(n2: Int)) =>
        _println("awake")
        _println(f"fixing difficulties of $n0, $n1, $n2")
        elvesRefs(n0).get.send(Helped())
        elvesRefs(n1).get.send(Helped())
        elvesRefs(n2).get.send(Helped())
        _println("sleeping")
        actions -= 1
  }

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        f(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    import collection.convert.ImplicitConversions._
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        val messages = ListBuffer(q.take)

        messages(0) match
          case IsBack(n: Int) =>
            reindeersBack(n) = true

            if reindeersBack.forall(r => r) then
              _println("awake")
              _println("delivering presents")
              reinDeerRefs.foreach(_.get.send(CanLeave()))
              reindeersBack = Array.fill[Boolean](reindeerNumber)(false)
              _println("sleeping")
              actions -= 1
          case _ =>
            q.drainTo(messages)
            val needHelps = messages.filter(_.isInstanceOf[NeedHelp])
            if needHelps.size >= 3 then
              // _println("awake")
              // _println(f"fixing difficulties of $n0, $n1, $n2")
              val consume = needHelps.take(3)
              consume
                .map(_.asInstanceOf[NeedHelp].n)
                .foreach((n: Int) => elvesRefs(n).get.send(Helped()))
              // _println("sleeping")
              (messages --= consume).foreach(q.put)
              actions -= 1
            else messages.foreach(q.put)

        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while actions > 0 do
      f(q)
      Thread.`yield`()
}

class Reindeer(val number: Int, var actions: Int) extends Benchmarkable[CanLeave, Unit] {
  private val q                       = LinkedTransferQueue[Msg]
  val ref                             = ActorRef(q)
  var santaRef: Option[ActorRef[Msg]] = None
  private var onHoliday               = true
  val isBack                          = () => !onHoliday
  private val _println: Any => Unit = (x: Any) =>
    println(f"${this.getClass.getSimpleName}[$number]: $x")

  private val f = receive { (y: Msg) =>
    y match
      case CanLeave() if isBack() =>
        assert(!onHoliday)
        onHoliday = true
        // _println("Going on holiday")
        actions -= 1
  }

  def comesBack() =
    if onHoliday then
      onHoliday = false
      _println("Came back")
      santaRef.get.send(IsBack(number))

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        comesBack()
        f(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        comesBack()
        val message = q.take

        if isBack() then
          assert(!onHoliday)
          onHoliday = true
          // _println("Going on holiday")
          actions -= 1
        else q.put(message)

        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while actions > 0 do
      comesBack()
      f(q)
      Thread.`yield`()
}

class Elf(val number: Int, var actions: Int) extends Benchmarkable[Helped, Unit] {
  private val q                       = LinkedTransferQueue[Msg]
  val ref                             = ActorRef(q)
  var santaRef: Option[ActorRef[Msg]] = None
  private var needHelp                = false
  var _needHelp                       = () => needHelp
  private val _println: Any => Unit = (x: Any) =>
    println(f"${this.getClass.getSimpleName}[$number]: $x")

  private val f = receive { (y: Msg) =>
    y match
      case Helped() if _needHelp() =>
        assert(needHelp)
        needHelp = false
        _println("Has been helped")
        actions -= 1
  }

  def askForHelp() =
    if !needHelp then
      needHelp = true
      _println("Needs help")
      santaRef.get.send(NeedHelp(number))

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        askForHelp()
        f(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        askForHelp()
        val message = q.take

        if _needHelp() then
          assert(needHelp)
          needHelp = false
          _println("Has been helped")
          actions -= 1
        else q.put(message)

        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while actions > 0 do
      askForHelp()
      f(q)
      Thread.`yield`()
}
