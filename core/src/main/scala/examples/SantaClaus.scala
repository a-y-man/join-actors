package examples

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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

def santaClausActor(elvesNumber: Int, algorithm: MatchingAlgorithm) =
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
            List(
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
          // println("awake")
          // println("delivering presents")
          // reinDeerRefs.foreach(_.get.send(CanLeave()))
          reinDeerRefs.foreach(_ ! CanLeave(selfRef))
          // println("sleeping")
          selfRef ! Rest()
          Next()
        case (NeedHelp(elfRef0), NeedHelp(elfRef1), NeedHelp(elfRef2)) =>
          // println("awake")
          // println(f"fixing difficulties of $n0, $n1, $n2")
          val elfRefs = List(elfRef0, elfRef1, elfRef2)
          elfRefs.foreach(_ ! Helped(selfRef))
          // println("sleeping")
          Next()
        case Rest() =>
          Stop(print("Santa is resting"))
    }(algorithm)
  }

  actor

def reindeerActor(algorithm: MatchingAlgorithm) =
  var onHoliday = true
  val isBack    = () => !onHoliday

  val actor = Actor[SAction, Unit] {
    receive { (y: SAction, _: ReindeerRef) =>
      y match
        case CanLeave(santaRef) =>
          // println("Going on holiday")
          onHoliday = false
          Next()
        case Rest() =>
          onHoliday = true
          Stop(print("Reindeer is resting"))
    }(algorithm)
  }

  // def comesBack() =
  //   onHoliday = false
  //   // println("Came back")
  //   santaRef.foreach(_ ! IsBack(reindeerRef))

  actor

def elfActor(algorithm: MatchingAlgorithm) =
  var needHelp  = false
  var _needHelp = () => needHelp

  val actor = Actor[SAction, Unit] {
    receive { (y: SAction, _: ElfRef) =>
      y match
        case Helped(santaRef) =>
          needHelp = false
          // println("Has been helped")
          Next()
        case Rest() =>
          Stop(print("Elf is resting"))
    }(algorithm)
  }

  // def askForHelp() =
  //   needHelp = true
  //   // println("Needs help")
  //   santaRef.foreach(_ ! NeedHelp(elfRef))

  actor
