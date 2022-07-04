package test.benchmark.santaClaus

import test.classes.santaClaus.{Elf, Reindeer, SantaClaus}
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}

def setup(
    reindeerNumber: Int,
    reindeerActions: Int,
    elvesNumber: Int,
    elvesActions: Int,
    santaActions: Int
): (Array[Elf], Array[Reindeer], SantaClaus) =
  val santa = SantaClaus(reindeerNumber, elvesNumber, santaActions)
  santa.actions = 2
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
  val reindeerNumber  = 9  // constant
  val reindeerActions = 4
  val elvesNumber     = 12 // multiple of 3
  val elvesActions    = 6
  val santaActions    = reindeerActions + ((elvesNumber * elvesActions) / 3)

  Benchmark(
    "Santa Claus",
    10,
    200,
    BenchmarkPass(
      "Control",
      () => {
        val (elves, reindeers, santa) =
          setup(reindeerNumber, reindeerActions, elvesNumber, elvesActions, santaActions)
        elves.foreach(_.run_without_macro)
        reindeers.foreach(_.run_without_macro)
        santa.run_without_macro
      }
    ),
    List(
      BenchmarkPass(
        "Macro",
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
