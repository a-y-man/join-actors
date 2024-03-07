package join_patterns.examples

import actor.*
import join_patterns.*

// import scala.util.*

enum ChameneoColor:
  case Blue, Red, Yellow

type ChameneoRef  = ActorRef[ChameneoMsg]
type MeetingPlace = ActorRef[MeetingPlaceMsg]

sealed trait ChameneoMsg
case class MeetMsg(ref: ChameneoRef, color: ChameneoColor) extends ChameneoMsg

sealed trait MeetingPlaceMsg
case class Meet(ref: MeetingPlace, color: ChameneoColor) extends MeetingPlaceMsg

def ChameneosExample(algorithm: MatchingAlgorithm, numberOfMeetings: Int, numberOfChameneos: Int) =

  val mallActor =
    Actor[MeetingPlaceMsg, Int] {
      receive { (x: MeetingPlaceMsg, mallRef: MeetingPlace) =>
        x match
          case (MeetMsg(ch1, c1), MeetMsg(ch2, c2)) if ch1 != ch2 && c1 == c2 =>
            ch1 ! MeetMsg(ch2, c2)
            Stop(1)
      }(algorithm)
    }
// TBC
