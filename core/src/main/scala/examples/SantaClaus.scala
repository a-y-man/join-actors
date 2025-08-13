package join_actors.examples

import join_actors.api.*

import scala.concurrent.Await
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
  val actor = Actor[SAction, Unit] {
    receive { (selfRef: SantaClausRef) =>
      {
        case IsBack(reindeerRef0) &:& IsBack(reindeerRef1) &:& IsBack(reindeerRef2)
            &:& IsBack(reindeerRef3) &:& IsBack(reindeerRef4) &:& IsBack(reindeerRef5)
            &:& IsBack(reindeerRef6) &:& IsBack(reindeerRef7) &:& IsBack(reindeerRef8) =>
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
          Continue
        case NeedHelp(elfRef0) &:& NeedHelp(elfRef1) &:& NeedHelp(elfRef2) =>
          println(s"${Console.RED}Ho Ho Ho! Let's help the elves!${Console.RESET}")
          val elfRefs = List(elfRef0, elfRef1, elfRef2)
          elfRefs.foreach(_ ! Helped(selfRef))
          Continue
        case Rest() =>
          Stop(())
      }
    }(matcher)
  }

  actor

def reindeerActor() = Actor[SAction, Unit] {
  receive { (_: ReindeerRef) =>
    {
      case CanLeave(_) =>
        println(s"${Console.YELLOW}Going on holiday${Console.RESET}")
        Continue
      case Rest() =>
        Stop(())
    }
  }(BruteForceMatcher)
}

def elfActor() = Actor[SAction, Unit] {
  receive { (_: ElfRef) =>
    {
      case Helped(_) =>
        println(s"${Console.GREEN}Has been helped${Console.RESET}")
        Continue
      case Rest() =>
        Stop(())
    }
  }(BruteForceMatcher)
}

def santaClausExample(matcher: MatcherFactory, santaActions: Int) =
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

  for _ <- 1 to santaActions do
    elfRefs foreach { e =>
      Thread.sleep(200)
      santaRef ! NeedHelp(e)
    }

    reindeerRefs foreach { r =>
      Thread.sleep(200)
      santaRef ! IsBack(r)
    }

  Future {
    Thread.sleep(200)
    reindeerRefs foreach { r =>
      r ! Rest()
    }

    elfRefs foreach { e =>
      e ! Rest()
    }
  }

  santaRef ! Rest()

  Await.result(santaActs, Duration.Inf)
