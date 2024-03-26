package join_patterns.examples

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
  val actor = Actor[SAction, Unit] {
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
          val reinDeerRefs =
            Array(
              reindeerRef0,
              reindeerRef1,
              reindeerRef2,
              reindeerRef3,
              reindeerRef4,
              reindeerRef5,
              reindeerRef6,
              reindeerRef7,
              reindeerRef8
            )
          println(
            s"${Console.RED}Ho Ho Ho! Let's prepare the sleigh for the reindeers!${Console.RESET}"
          )
          reinDeerRefs.foreach(_ ! CanLeave(selfRef))
          Next()
        case (NeedHelp(elfRef0), NeedHelp(elfRef1), NeedHelp(elfRef2)) =>
          println(s"${Console.RED}Ho Ho Ho! Let's help the elves!${Console.RESET}")
          val elfRefs = List(elfRef0, elfRef1, elfRef2)
          elfRefs.foreach(_ ! Helped(selfRef))
          Next()
        case Rest() =>
          Stop(())
    }(algorithm)
  }

  actor

def reindeerActor() = Actor[SAction, Unit] {
  receive { (y: SAction, _: ReindeerRef) =>
    y match
      case CanLeave(_) =>
        println(s"${Console.YELLOW}Going on holiday${Console.RESET}")
        Next()
      case Rest() =>
        Stop(())
  }(MatchingAlgorithm.BruteForceAlgorithm)
}

def elfActor() = Actor[SAction, Unit] {
  receive { (y: SAction, _: ElfRef) =>
    y match
      case Helped(_) =>
        println(s"${Console.GREEN}Has been helped${Console.RESET}")
        Next()
      case Rest() =>
        Stop(())
  }(MatchingAlgorithm.BruteForceAlgorithm)
}

def santaClausExample(algorithm: MatchingAlgorithm) =
  implicit val ec = ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

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
    elfRefs foreach { e =>
      santaRef ! NeedHelp(e)
    }
  }

  Future {
    reindeerRefs foreach { r =>
      santaRef ! IsBack(r)
    }
  }

  santaRef ! Rest()

  Await.ready(santaActs, Duration.Inf)

  reindeerRefs foreach { r =>
    r ! Rest()
  }

  elfRefs foreach { e =>
    e ! Rest()
  }
