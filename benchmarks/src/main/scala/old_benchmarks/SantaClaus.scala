package old_benchmarks

import join_actors.api.*
import os.Path

import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

type SantaClausRef = ActorRef[NeedHelp | IsBack | Rest]
type ReindeerRef = ActorRef[CanLeave | Rest]
type ElfRef = ActorRef[Helped | Rest]

sealed trait SAction
case class IsBack(reindeerRef: ReindeerRef) extends SAction
case class CanLeave(santaRef: SantaClausRef) extends SAction
case class Helped(santaRef: SantaClausRef) extends SAction
case class NeedHelp(elfRef: ElfRef) extends SAction
case class Rest() extends SAction

val N_REINDEERS = 9

val N_ELVES = 3

def santaClausActor(matcher: MatcherFactory) =
  var actions = 0

  val actor = Actor[SAction, (Long, Int)] {
    receive { (selfRef: SantaClausRef) =>
      {
        case (
              IsBack(reindeerRef0),
              IsBack(reindeerRef1),
              IsBack(reindeerRef2),
              IsBack(reindeerRef3),
              IsBack(reindeerRef4),
              IsBack(reindeerRef5),
              IsBack(reindeerRef6),
              IsBack(reindeerRef7),
              IsBack(reindeerRef8)
            ) =>
          reindeerRef0 ! CanLeave(selfRef)
          reindeerRef1 ! CanLeave(selfRef)
          reindeerRef2 ! CanLeave(selfRef)
          reindeerRef3 ! CanLeave(selfRef)
          reindeerRef4 ! CanLeave(selfRef)
          reindeerRef5 ! CanLeave(selfRef)
          reindeerRef6 ! CanLeave(selfRef)
          reindeerRef7 ! CanLeave(selfRef)
          reindeerRef8 ! CanLeave(selfRef)
          actions += 1
          Continue
        case (NeedHelp(elfRef0), NeedHelp(elfRef1), NeedHelp(elfRef2)) =>
          elfRef0 ! Helped(selfRef)
          elfRef1 ! Helped(selfRef)
          elfRef2 ! Helped(selfRef)
          actions += 1
          Continue
        case Rest() =>
          Stop((System.currentTimeMillis(), actions))
      }
    }(matcher)
  }

  actor

def reindeerActor() =
  var actions = 0
  Actor[SAction, (Long, Int)] {
    receive { (_: ReindeerRef) =>
      {
        case CanLeave(_) =>
          actions += 1
          Continue
        case Rest() =>
          Stop((System.currentTimeMillis(), actions))
      }
    }(BruteForceMatcher)
  }

def elfActor() =
  var actions = 0
  Actor[SAction, (Long, Int)] {
    receive { (_: ElfRef) =>
      {
        case Helped(santaRef) =>
          actions += 1
          Continue
        case Rest() =>
          Stop((System.currentTimeMillis(), actions))
      }
    }(BruteForceMatcher)
  }

def measureSantaClaus(santaClauseActions: Int, matcher: MatcherFactory): Future[Measurement] =
  val reindeers = (0 to N_REINDEERS - 1).map { i =>
    reindeerActor().start()
  }.toArray

  val elves = (0 to N_ELVES - 1).map { i =>
    elfActor().start()
  }.toArray

  val santa = santaClausActor(matcher)

  val (santaActs, santaRef) = santa.start()

  val elfRefs = elves map { e =>
    e._2
  }

  val reindeerRefs = reindeers map { r =>
    r._2
  }

  Future {
    val startTime = System.currentTimeMillis()

    for _ <- 1 to santaClauseActions do
      reindeerRefs foreach { r =>
        santaRef ! IsBack(r)
      }

      elfRefs foreach { e =>
        santaRef ! NeedHelp(e)
      }

    santaRef ! Rest()

    reindeerRefs foreach { r =>
      r ! Rest()
    }

    elfRefs foreach { e =>
      e ! Rest()
    }

    Await.ready(Future.sequence(reindeers.map(_._1) ++ elves.map(_._1)), Duration.Inf)

    val (endTime, matches) = Await.result(santaActs, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }

def santaClausBenchmark(santaClauseActions: Int, matcher: MatcherFactory) =
  val nullPass = measureSantaClaus(santaClauseActions, matcher)
  Benchmark(
    name = "Santa Claus",
    matcher = matcher,
    warmupRepetitions = 5,
    repetitions = 5,
    nullPass = BenchmarkPass(
      "Null Pass",
      () => nullPass
    ),
    passes = List(
      BenchmarkPass(
        "Santa Claus",
        () => measureSantaClaus(santaClauseActions, matcher)
      )
    )
  )

def runSantaClausBenchmark(
    santaClauseActions: Int,
    writeToFile: Boolean = false,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val matchers: List[MatcherFactory] =
    List(
      StatefulTreeMatcher,
      MutableStatefulMatcher,
      LazyMutableMatcher,
      WhileLazyMatcher,
      FilteringWhileMatcher,
      WhileEagerMatcher,
      ArrayWhileMatcher,
      EagerParallelMatcher(2),
      EagerParallelMatcher(4),
      EagerParallelMatcher(6),
      EagerParallelMatcher(8),
      LazyParallelMatcher(2),
      LazyParallelMatcher(4),
      LazyParallelMatcher(6),
      LazyParallelMatcher(8),
      FilteringParallelMatcher(2),
      FilteringParallelMatcher(4),
      FilteringParallelMatcher(6),
      FilteringParallelMatcher(8),
      ArrayParallelMatcher(2),
      ArrayParallelMatcher(4),
      ArrayParallelMatcher(6),
      ArrayParallelMatcher(8)
    )

  val measurements = matchers map { matcher =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $matcher${Console.RESET}"
    )
    val measurement = santaClausBenchmark(santaClauseActions, matcher).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $matcher finished${Console.RESET}"
    )

    (matcher, measurement)
  }

  if writeToFile then saveToFile("SantaClaus", measurements, outputDataDir)
