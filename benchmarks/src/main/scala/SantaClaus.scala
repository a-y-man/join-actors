package benchmarks

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

type SantaClausRef = ActorRef[NeedHelp | IsBack | Rest]
type ReindeerRef   = ActorRef[CanLeave | Rest]
type ElfRef        = ActorRef[Helped | Rest]

sealed trait SAction
case class IsBack(reindeerRef: ReindeerRef)  extends SAction
case class CanLeave(santaRef: SantaClausRef) extends SAction
case class Helped(santaRef: SantaClausRef)   extends SAction
case class NeedHelp(elfRef: ElfRef)          extends SAction
case class Rest()                            extends SAction

val N_REINDEERS = 9

val N_ELVES = 3

def santaClausActor(algorithm: MatchingAlgorithm) =
  var actions = 0

  val actor = Actor[SAction, (Long, Int)] {
    receive { (y: SAction, selfRef: SantaClausRef) =>
      y match
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
          Next()
        case (NeedHelp(elfRef0), NeedHelp(elfRef1), NeedHelp(elfRef2)) =>
          elfRef0 ! Helped(selfRef)
          elfRef1 ! Helped(selfRef)
          elfRef2 ! Helped(selfRef)
          actions += 1
          Next()
        case Rest() =>
          Stop((System.currentTimeMillis(), actions))
    }(algorithm)
  }

  actor

def reindeerActor() =
  var actions = 0
  Actor[SAction, (Long, Int)] {
    receive { (y: SAction, _: ReindeerRef) =>
      y match
        case CanLeave(_) =>
          actions += 1
          Next()
        case Rest() =>
          Stop((System.currentTimeMillis(), actions))
    }(MatchingAlgorithm.BruteForceAlgorithm)
  }

def elfActor() =
  var actions = 0
  Actor[SAction, (Long, Int)] {
    receive { (y: SAction, _: ElfRef) =>
      y match
        case Helped(santaRef) =>
          actions += 1
          Next()
        case Rest() =>
          Stop((System.currentTimeMillis(), actions))
    }(MatchingAlgorithm.BruteForceAlgorithm)
  }

def measureSantaClaus(santaClauseActions: Int, algorithm: MatchingAlgorithm): Future[Measurement] =
  val reindeers = (0 to N_REINDEERS - 1).map { i =>
    reindeerActor().start()
  }.toArray

  val elves = (0 to N_ELVES - 1).map { i =>
    elfActor().start()
  }.toArray

  val santa = santaClausActor(algorithm)

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
      elfRefs foreach { e =>
        santaRef ! NeedHelp(e)
      }

      reindeerRefs foreach { r =>
        santaRef ! IsBack(r)
      }

    santaRef ! Rest()

    reindeerRefs foreach { r =>
      r ! Rest()
    }

    elfRefs foreach { e =>
      e ! Rest()
    }

    val (endTime, matches) = Await.result(santaActs, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }
