package examples

// import actor.*
// import join_patterns.*
// import org.scalacheck.*
// import org.scalatest.run

// import java.util.concurrent.TimeUnit
// import scala.compiletime.ops.int
// import scala.concurrent.Await
// import scala.concurrent.Future
// import scala.concurrent.duration.Duration
// import scala.util.*

// import concurrent.ExecutionContext.Implicits.global

object Chameneos extends App:
  println("Chameneos")
  // type ChameneoRef  = ActorRef[ChameneoMsg]
  // type MeetingPlace = ActorRef[MeetingPlaceMsg]

  // sealed trait ChameneoMsg
  // case class Chameneo(ref: ChameneoRef, color: Color) extends ChameneoMsg

  // sealed trait MeetingPlaceMsg
  // case class Meet(ref: MeetingPlace, color: Color) extends MeetingPlaceMsg

  // sealed trait Color
  // case object Blue   extends Color
  // case object Red    extends Color
  // case object Yellow extends Color

  // val ALGORITHM = MatchingAlgorithm.BasicAlgorithm

  // val mallActor =
  //   Actor[MeetingPlaceMsg, Int](receive { (x: MeetingPlaceMsg, mallRef: MeetingPlace) =>
  //     x match
  //       case (Chameneo(ch1, c1), Chameneo(ch2, c2)) if ch1 != ch2 && c1 == c2 =>
  //         ch1 ! Chameneo(ch2, c2)
  //         Stop(1)
  //   }(ALGORITHM))
