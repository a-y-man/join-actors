package test.benchmark.santaClaus

import test.ALGORITHM
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass
import test.classes.Msg
import test.classes.santaClaus.Elf
import test.classes.santaClaus.Reindeer
import test.classes.santaClaus.SantaClaus

import scala.concurrent.Await

def setup(
    reindeerNumber: Int,
    reindeerActions: Int,
    elvesNumber: Int,
    elvesActions: Int,
    santaActions: Int
): (Array[Elf], Array[Reindeer], SantaClaus) =
  val santa = SantaClaus(elvesNumber, santaActions)
  val elves = (0 to elvesNumber - 1).map { i =>
    val e = Elf(i, elvesActions)
    santa.elvesRefs.update(i, Some(e.ref))
    e.santaRef = Some(santa.ref)
    e
  }.toArray
  val reindeers = (0 to reindeerNumber - 1).map { i =>
    val r = Reindeer(i, reindeerActions)
    santa.reinDeerRefs.update(i, Some(r.ref))
    r.santaRef = Some(santa.ref)
    r
  }.toArray

  (elves, reindeers, santa)

@main
def santaClausBenchmark =
  val reindeerNumber  = 9 // constant
  val reindeerActions = 15
  val elvesNumber     = 6 // multiple of 3
  val elvesActions    = 15
  val santaActions    = reindeerActions + ((elvesNumber * elvesActions) / 3)
  Benchmark(
    "Santa Claus",
    10,
    100,
    BenchmarkPass(
      "Control",
      () => {
        val (elves, reindeers, santa) =
          setup(reindeerNumber, reindeerActions, elvesNumber, elvesActions, santaActions)
        elves.foreach(_.run_as_future)
        reindeers.foreach(_.run_as_future)
        santa.run_as_future
      }
    ),
    List(
      BenchmarkPass(
        s"Macro using ${ALGORITHM.toString()}",
        () => {
          val (elves, reindeers, santa) =
            setup(reindeerNumber, reindeerActions, elvesNumber, elvesActions, santaActions)
          elves.foreach(_.run_as_future)
          reindeers.foreach(_.run_as_future)
          santa.run_as_future
        }
      )
    )
  ).run
