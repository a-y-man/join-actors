package test.classes.santaClaus

import scala.concurrent.{Future, ExecutionContext}
import scala.collection.mutable.ListBuffer

import test.classes.Msg
import test.benchmark.Benchmarkable
import join_patterns.receive
import actor.ActorRef
import test.ALGORITHM

case class IsBack()         extends Msg
case class CanLeave()       extends Msg
case class Helped()         extends Msg
case class NeedHelp(n: Int) extends Msg

val N_REINDEERS = 9

class SantaClaus(
    val elvesNumber: Int,
    var actions: Int
) extends Benchmarkable[Msg, Unit]:
  val reinDeerRefs                  = Array.fill[Option[ActorRef[CanLeave]]](N_REINDEERS)(None)
  val elvesRefs                     = Array.fill[Option[ActorRef[Helped]]](elvesNumber)(None)
  private val _println: Any => Unit = (x: Any) => println(f"${this.getClass.getSimpleName}: $x")

  protected val matcher = receive { (y: Msg) =>
    y match
      case (
            IsBack(),
            N_REINDEERS
          ) =>
        // _println("awake")
        // _println("delivering presents")
        // reinDeerRefs.foreach(_.get.send(CanLeave()))
        reinDeerRefs.foreach(_.map(_ ! CanLeave()))
        // _println("sleeping")
        actions -= 1
      case (NeedHelp(n0: Int), NeedHelp(n1: Int), NeedHelp(n2: Int)) =>
        // _println("awake")
        // _println(f"fixing difficulties of $n0, $n1, $n2")
        elvesRefs(n0).foreach(_ ! Helped())
        elvesRefs(n1).foreach(_ ! Helped())
        elvesRefs(n2).foreach(_ ! Helped())
        // _println("sleeping")
        actions -= 1
  }(ALGORITHM)

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        // _println(f"match: $actions")
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while actions > 0 do
      matcher(q)
      Thread.`yield`()

class Reindeer(val number: Int, var actions: Int) extends Benchmarkable[CanLeave, Unit]:
  var santaRef: Option[ActorRef[Msg]] = None
  private var onHoliday               = true
  val isBack                          = () => !onHoliday
  private val _println: Any => Unit = (x: Any) =>
    println(f"${this.getClass.getSimpleName}[$number]: $x")

  protected val matcher = receive { (y: Msg) =>
    y match
      case CanLeave() =>
        // _println("Going on holiday")
        actions -= 1
  }(ALGORITHM)

  def comesBack() =
    onHoliday = false
    // _println("Came back")
    santaRef.foreach(_ ! IsBack())

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        comesBack()
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while actions > 0 do
      comesBack()
      matcher(q)
      Thread.`yield`()

class Elf(val number: Int, var actions: Int) extends Benchmarkable[Helped, Unit]:
  var santaRef: Option[ActorRef[Msg]] = None
  private var needHelp                = false
  var _needHelp                       = () => needHelp
  private val _println: Any => Unit = (x: Any) =>
    println(f"${this.getClass.getSimpleName}[$number]: $x")

  protected val matcher = receive { (y: Msg) =>
    y match
      case Helped() =>
        needHelp = false
        // _println("Has been helped")
        actions -= 1
  }(ALGORITHM)

  def askForHelp() =
    needHelp = true
    // _println("Needs help")
    santaRef.get.send(NeedHelp(number))

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while actions > 0 do
        askForHelp()
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while actions > 0 do
      askForHelp()
      matcher(q)
      Thread.`yield`()
