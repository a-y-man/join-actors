package test.classes.santaClaus

import java.util.concurrent.LinkedTransferQueue

import join_patterns.{ActorRef, receive}
import test.classes.Msg

case class IsBack(n: Int)   extends Msg
case class CanLeave()       extends Msg
case class Helped()         extends Msg
case class NeedHelp(n: Int) extends Msg

class SantaClaus(val reindeerNumber: Int, val elvesNumber: Int) extends Runnable {
  private val q             = LinkedTransferQueue[Msg]
  val ref                   = ActorRef(q)
  val reinDeerRefs          = Array.fill[Option[ActorRef[Msg]]](reindeerNumber)(None)
  val elvesRefs             = Array.fill[Option[ActorRef[Msg]]](elvesNumber)(None)
  private var reindeersBack = 0
  private val isBackAndCheck: Int => Boolean = (n: Int) =>
    reindeersBack += 1
    reindeersBack == reindeerNumber

  /** Set to a positive number so it stops after a ceratin number of actions. Set to any negative
    * number to it loops forever.
    */
  var actions = 0

  val _println: Any => Unit = (x: Any) => println(f"${this.getClass.getSimpleName}: $x")

  private def f = receive { (y: Msg) =>
    y match
      case IsBack(n: Int) =>
        // not as guard, to consume the message
        if isBackAndCheck(n) then
          _println("awake")
          _println("delivering presents")
          reinDeerRefs.foreach(_.get.send(CanLeave()))
          reindeersBack = 0
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

  override def run =
    while actions != 0 do
      f(q)
      Thread.`yield`()
}

class Reindeer(val number: Int) extends Runnable {
  private val q                       = LinkedTransferQueue[Msg]
  val ref                             = ActorRef(q)
  var santaRef: Option[ActorRef[Msg]] = None
  var onHoliday                       = true
  val isBack                          = () => !onHoliday
  var isDone                          = false

  val _println: Any => Unit = (x: Any) => println(f"${this.getClass.getSimpleName}[$number]: $x")

  private def f = receive { (y: Msg) =>
    y match
      case CanLeave() if isBack() =>
        assert(!onHoliday)
        onHoliday = true
        _println("Going on holiday")
        isDone = true
  }

  def comesBack() =
    if onHoliday then
      onHoliday = false
      _println("Came back")
      santaRef.get.send(IsBack(number))

  override def run =
    comesBack()
    while !isDone do
      f(q)
      Thread.`yield`()
}

class Elf(val number: Int) extends Runnable {
  private val q                       = LinkedTransferQueue[Msg]
  val ref                             = ActorRef(q)
  var santaRef: Option[ActorRef[Msg]] = None
  var needHelp                        = false
  var _needHelp                       = () => needHelp
  var isDone                          = false

  val _println: Any => Unit = (x: Any) => println(f"${this.getClass.getSimpleName}[$number]: $x")

  private def f = receive { (y: Msg) =>
    y match
      case Helped() if _needHelp() =>
        assert(needHelp)
        needHelp = false
        _println("Has been helped")
        isDone = true
  }

  def askForHelp() =
    if !needHelp then
      needHelp = true
      _println("Needs help")
      santaRef.get.send(NeedHelp(number))

  override def run =
    askForHelp()
    while !isDone do
      f(q)
      Thread.`yield`()
}
