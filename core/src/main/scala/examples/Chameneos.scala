package join_actors.examples

import join_actors.api.*
import org.scalacheck.Gen

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

enum ChameneoColor:
  case Blue
  case Red
  case Yellow

  def complement(that: ChameneoColor): ChameneoColor =
    import ChameneoColor.*
    (this, that) match
      case (Blue, Red) => Yellow
      case (Blue, Yellow) => Red
      case (Red, Blue) => Yellow
      case (Red, Yellow) => Blue
      case (Yellow, Blue) => Red
      case (Yellow, Red) => Blue
      case (Blue, Blue) => Blue
      case (Red, Red) => Red
      case (Yellow, Yellow) => Yellow

type ChameneoRef = ActorRef[ChameneosMsg]
type MeetingPlace = ActorRef[ChameneosMsg]

enum ChameneosMsg:
  case Start()
  case MeetMsg(ref: ChameneoRef, color: ChameneoColor)
  case Exit()

case class ChameneosConfig(
    maxNumberOfMeetings: Int,
    numberOfChameneos: Int,
    matcher: MatcherFactory
)

def chameneoActor(
    initColor: ChameneoColor,
    mall: MeetingPlace,
    matcher: MatcherFactory
) =
  import ChameneoColor.*, ChameneosMsg.*
  val color = initColor
  Actor[ChameneosMsg, Unit] {
    receive { (thisChameneo: ChameneoRef) =>
      {
        case Start() =>
          mall ! MeetMsg(thisChameneo, color)
          Continue
        case MeetMsg(otherChameneo, otherColor) =>
          val newColor = color.complement(otherColor)
          otherChameneo ! MeetMsg(thisChameneo, newColor)
          mall ! MeetMsg(thisChameneo, color)
          mall ! MeetMsg(otherChameneo, otherColor)
          Continue
        case Exit() =>
          Stop(())
      }
    }(matcher)
  }

def mallActor(maxNumberOfMeetings: Int, matcher: MatcherFactory) =
  import ChameneoColor.*, ChameneosMsg.*
  var meetings = 0
  Actor[ChameneosMsg, Int] {
    receive { (mallRef: MeetingPlace) =>
      {
        case MeetMsg(ch1, c1) &:& MeetMsg(ch2, c2) if c1 != c2 && meetings < maxNumberOfMeetings =>
          println(s"Meeting: $c1, $c2 --- $meetings")
          ch1 ! MeetMsg(ch2, c2)
          meetings += 1
          if meetings >= maxNumberOfMeetings then mallRef ! Exit()
          println(s"meetings: $meetings")
          Continue
        case Exit() if meetings >= maxNumberOfMeetings =>
          Stop(meetings)
      }
    }(matcher)
  }

def chameneosExample(
    chameneosConfig: ChameneosConfig
) =
  import ChameneoColor.*, ChameneosMsg.*

  println(
    s"Chameneos Example: ${chameneosConfig}"
  )

  val (mallFut, mallRef) =
    mallActor(
      chameneosConfig.maxNumberOfMeetings,
      chameneosConfig.matcher
    ).start()

  val colors = Gen.oneOf(List(Blue, Red, Yellow))

  val chameneos = (1 to chameneosConfig.numberOfChameneos).map { i =>
    chameneoActor(
      colors.sample.get,
      mallRef,
      chameneosConfig.matcher
    ).start()
  }

  chameneos foreach { c =>
    val (fut, ref) = c
    ref ! Start()
  }

  val meetings = Await.result(mallFut, Duration(5, "minutes"))

  chameneos foreach { c =>
    val (fut, ref) = c
    println(s"Sending Exit to $ref")
    ref ! Exit()
    Await.ready(fut, Duration.Inf)
  }

  println(s"Meetings: $meetings")
