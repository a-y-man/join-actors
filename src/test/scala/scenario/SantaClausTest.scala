package test.scenario.santaClaus

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

import test.classes.santaClaus._

class SantaClausTest extends AnyFunSuite {
  test("Elves are helped") {
    val elvesNumber = 3

    val santa = SantaClaus(elvesNumber, 1)
    val elves = (0 to elvesNumber - 1).map { i =>
      val e = Elf(i, 1)
      santa.elvesRefs.update(i, Some(e.ref))
      e.santaRef = Some(santa.ref)
      e
    }.toArray

    val elfThreads  = elves.map(Thread(_))
    val santaThread = Thread(santa)

    santaThread.start
    elfThreads.foreach(_.start)

    santaThread.join
    elfThreads.foreach(_.join)

    assert(elves.forall(!_._needHelp()))
  }

  test("Reindeers come back") {
    val santa = SantaClaus(0, 1)
    val reindeers = (0 to N_REINDEERS - 1).map { i =>
      val r = Reindeer(i, 1)
      santa.reinDeerRefs.update(i, Some(r.ref))
      r.santaRef = Some(santa.ref)
      r
    }.toArray

    val reindeerThreads = reindeers.map(Thread(_))
    val santaThread     = Thread(santa)

    santaThread.start
    reindeerThreads.foreach(_.start)

    santaThread.join
    reindeerThreads.foreach(_.join)

    assert(reindeers.forall(!_.isBack()))
  }

  test("Reindeers have priority over elves") {
    val elvesNumber = 3

    val santa = SantaClaus(elvesNumber, 2)
    val elves = (0 to elvesNumber - 1).map { i =>
      val e = Elf(i, 1)
      santa.elvesRefs.update(i, Some(e.ref))
      e.santaRef = Some(santa.ref)
      e
    }.toArray
    val reindeers = (0 to N_REINDEERS - 1).map { i =>
      val r = Reindeer(i, 1)
      santa.reinDeerRefs.update(i, Some(r.ref))
      r.santaRef = Some(santa.ref)
      r
    }.toArray

    val elfThreads      = elves.map(Thread(_))
    val reindeerThreads = reindeers.map(Thread(_))
    val santaThread     = Thread(santa)

    elfThreads.foreach(_.start)
    Thread.sleep(1000) // elves' messages are first in the queue
    reindeerThreads.foreach(_.start)
    Thread.sleep(1000) // all reindeers' messages are in the queue
    santaThread.start

    santaThread.join
    reindeerThreads.foreach(_.join)
    elfThreads.foreach(_.join)

    assert(elves.forall(!_._needHelp()))
    assert(reindeers.forall(!_.isBack()))
  }

  test("Preserve Elves' help order") {
    val elvesNumber = 6

    val santa = SantaClaus(elvesNumber, 2)
    val elves = (0 to elvesNumber - 1).map { i =>
      val e = Elf(i, 1)
      santa.elvesRefs.update(i, Some(e.ref))
      e.santaRef = Some(santa.ref)
      e
    }.toArray

    val elfThreads  = elves.map(Thread(_))
    val santaThread = Thread(santa)

    elfThreads.reverse.foreach(_.start)
    santaThread.start

    santaThread.join
    elfThreads.foreach(_.join)

    assert(elves.forall(!_._needHelp()))
  }
}
