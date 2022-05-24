package join_patterns

import org.scalatest.funsuite._
import java.util.concurrent.LinkedTransferQueue

class SantaClausTest extends AnyFunSuite {
  sealed abstract class Msg
  case class IsBack(n: Int) extends Msg
  case class CanLeave() extends Msg
  case class Helped(name: String) extends Msg
  case class NeedHelp(name: String) extends Msg

  val reindeerNumber = 9

  class SantaClaus(q: LinkedTransferQueue[Msg]) {
    var reindeersBack = 0
    val isBackAndCheck: Int => Boolean = (n: Int) =>
      reindeersBack += 1
      reindeersBack == 9

    val _println: Any => Unit = (x: Any) =>
      print(f"${this.getClass.getSimpleName}: ")
      println(x)

    val f = receive { (y: Msg) => y match
      case (NeedHelp(n0: String), NeedHelp(n1: String), NeedHelp(n2: String), IsBack(n: Int))
        if isBackAndCheck(n) =>
        _println("awake")
        q.put(NeedHelp(n0))
        q.put(NeedHelp(n1))
        q.put(NeedHelp(n2))
        _println("delivering presents")
        (1 to reindeerNumber).foreach(_ => q.put(CanLeave()))
        reindeersBack = 0
        _println("sleeping")
      case (NeedHelp(n0: String), NeedHelp(n1: String), NeedHelp(n2: String)) =>
        _println("awake")
        _println(f"fixing difficulties of $n0, $n1, $n2")
        q.put(Helped(n0))
        q.put(Helped(n1))
        q.put(Helped(n2))
        _println("sleeping")
      case IsBack(n: Int) if isBackAndCheck(n) =>
        _println("awake")
        _println("delivering presents")
        (1 to reindeerNumber).foreach(_ => q.put(CanLeave()))
        reindeersBack = 0
        _println("sleeping")
    }
  }

  class Reindeer(val number: Int) {
    var onHoliday = true
    val isNotOnHoliday = () => !onHoliday

    val _println: Any => Unit = (x: Any) =>
      print(f"${this.getClass.getSimpleName}[$number]: ")
      println(x)

    val f = receive { (y: Msg) => y match
      case CanLeave() if isNotOnHoliday() =>
        assert(!onHoliday)
        onHoliday = true
        _println("Going on holiday")
    }

    def comesBack(q: LinkedTransferQueue[Msg]) =
      if onHoliday then
        onHoliday = false
        _println("Came back")
        q.put(IsBack(number))
  }

  class Elf(val name: String) {
    var needHelp = false
    val sameName = (s: String) => s == name

    val _println: Any => Unit = (x: Any) =>
      print(f"${this.getClass.getSimpleName}[$name]: ")
      println(x)

    val f = receive { (y: Msg) => y match
      case Helped(s: String) if sameName(s) =>
        assert(needHelp)
        needHelp = false
        _println("Has been helped")
    }

    def askForHelp(q: LinkedTransferQueue[Msg]) =
      if !needHelp then
        needHelp = true
        _println("Needs help")
        q.put(NeedHelp(name))
  }

  test("Elves are helped") {
    val q = LinkedTransferQueue[Msg]
    val santa = SantaClaus(q)
    val elves = Array(
      "Fingon", "Turgon", "Aredhel", "Argon", "Finrod", "Angrod", "Aegnor", "Galadriel"
    ).toSet.map(Elf(_)).toArray

    elves(0).askForHelp(q)
    elves(1).askForHelp(q)
    elves(2).askForHelp(q)

    assert(elves.zipWithIndex.forall((e, i) =>
      if i == 0 || i == 1 || i == 2 then e.needHelp else !e.needHelp
    ))

    santa.f(q)
    elves(0).f(q)
    elves(1).f(q)
    elves(2).f(q)

    assert(elves.forall(!_.needHelp))
  }

  test("Reindeers come back") {
    val q = LinkedTransferQueue[Msg]
    val santa = SantaClaus(q)
    val reindeers: Array[Reindeer] = Array.tabulate(reindeerNumber)(Reindeer(_))

    reindeers.foreach(_.comesBack(q))

    assert(reindeers.forall(!_.onHoliday))

    santa.f(q)
    reindeers.foreach(_.f(q))

    assert(reindeers.forall(_.onHoliday))
  }

  test("Reindeers have priority over elves") {
    val q = LinkedTransferQueue[Msg]
    val santa = SantaClaus(q)
    val reindeers: Array[Reindeer] = Array.tabulate(reindeerNumber)(Reindeer(_))
    val elves = Array(
      "Fingon", "Turgon", "Aredhel", "Argon", "Finrod", "Angrod", "Aegnor", "Galadriel"
    ).toSet.map(Elf(_)).toArray

    elves(3).askForHelp(q)
    elves(4).askForHelp(q)
    elves(5).askForHelp(q)
    reindeers.foreach(_.comesBack(q))

    assert(reindeers.forall(!_.onHoliday))
    assert(elves.zipWithIndex.forall((e, i) =>
      if i == 3 || i == 4 || i == 5 then e.needHelp else !e.needHelp
    ))

    santa.f(q)
    reindeers.foreach(_.f(q))

    assert(reindeers.forall(_.onHoliday))
    assert(elves.zipWithIndex.forall((e, i) =>
      if i == 3 || i == 4 || i == 5 then e.needHelp else !e.needHelp
    ))

    santa.f(q)
    elves(3).f(q)
    elves(4).f(q)
    elves(5).f(q)

    assert(elves.forall(!_.needHelp))
  }

  test("Preserve Elves' help order") {
    val q = LinkedTransferQueue[Msg]
    val santa = SantaClaus(q)
    val reindeers: Array[Reindeer] = Array.tabulate(reindeerNumber)(Reindeer(_))
    val elves = Array(
      "Aredhel", "Galadriel", "Fingon", "Turgon", "Argon", "Finrod", "Angrod", "Aegnor"
    ).map(Elf(_)).toSet.toList

    elves(4).askForHelp(q)
    elves(5).askForHelp(q)
    elves(6).askForHelp(q)
    elves(0).askForHelp(q)
    elves(1).askForHelp(q)
    elves(7).askForHelp(q)

    santa.f(q)
    elves(4).f(q)
    elves(5).f(q)
    elves(6).f(q)

    assert(elves.zipWithIndex.forall((e, i) =>
      if i == 0 || i == 1 || i == 7 then e.needHelp else !e.needHelp
    ))
  }
}
