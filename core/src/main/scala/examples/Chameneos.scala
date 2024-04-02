package join_patterns.examples

import actor.*
import join_patterns.*
import org.scalacheck.Gen

import scala.concurrent.Await
import scala.concurrent.duration.Duration

enum ChameneoColor:
  case Blue
  case Red
  case Yellow

  def complement(that: ChameneoColor): ChameneoColor =
    import ChameneoColor.*
    (this, that) match
      case (Blue, Red)      => Yellow
      case (Blue, Yellow)   => Red
      case (Red, Blue)      => Yellow
      case (Red, Yellow)    => Blue
      case (Yellow, Blue)   => Red
      case (Yellow, Red)    => Blue
      case (Blue, Blue)     => Blue
      case (Red, Red)       => Red
      case (Yellow, Yellow) => Yellow

type ChameneoRef  = ActorRef[ChameneosMsg]
type MeetingPlace = ActorRef[ChameneosMsg]

enum ChameneosMsg:
  case Start()
  case MeetMsg(ref: ChameneoRef, color: ChameneoColor)
  case Exit()

def chameneoActor(
    initColor: ChameneoColor,
    mall: MeetingPlace,
    algorithm: MatchingAlgorithm
) =
  import ChameneoColor.*, ChameneosMsg.*
  val color = initColor
  Actor[ChameneosMsg, Unit] {
    receive { (x: ChameneosMsg, thisChameneo: ChameneoRef) =>
      x match
        case Start() =>
          mall ! MeetMsg(thisChameneo, color)
          Next()
        case MeetMsg(otherChameneo, otherColor) =>
          val newColor = color.complement(otherColor)
          otherChameneo ! MeetMsg(thisChameneo, newColor)
          mall ! MeetMsg(thisChameneo, color)
          mall ! MeetMsg(otherChameneo, otherColor)
          Next()
        case Exit() =>
          Stop(())
    }(algorithm)
  }

def mallActor(maxNumberOfMeetings: Int, algorithm: MatchingAlgorithm) =
  import ChameneoColor.*, ChameneosMsg.*
  var meetings = 0

  Actor[ChameneosMsg, Int] {
    receive { (x: ChameneosMsg, mallRef: MeetingPlace) =>
      x match
        case (MeetMsg(ch1, c1), MeetMsg(ch2, c2)) if ch1 != ch2 =>
          if meetings < maxNumberOfMeetings then
            println(s"Meeting: $c1, $c2 --- $meetings")
            ch1 ! MeetMsg(ch2, c2)
            meetings += 1
            Next()
          else
            ch1 ! Exit()
            ch2 ! Exit()
            mallRef ! Exit()
            Next()
        case Exit() =>
          Stop(meetings)
    }(algorithm)
  }

def chameneosExample(
    maxNumberOfMeetings: Int,
    numberOfChameneos: Int,
    algorithm: MatchingAlgorithm
) =
  import ChameneoColor.*, ChameneosMsg.*

  println(s"Chameneos Example: $maxNumberOfMeetings, $numberOfChameneos, $algorithm")

  val (mallFut, mallRef) = mallActor(maxNumberOfMeetings, algorithm).start()

  // randomly assign colors to chameneos
  val colors = Gen.oneOf(List(Blue, Red, Yellow))

  val chameneos = (0 until numberOfChameneos).map { i =>
    chameneoActor(
      colors.sample.get,
      mallRef,
      algorithm
    )
  } map { c =>
    val (fut, ref) = c.start()
    ref ! Start()
  }

  val meetings = Await.result(mallFut, Duration.Inf)

  println(s"Meetings: $meetings")
