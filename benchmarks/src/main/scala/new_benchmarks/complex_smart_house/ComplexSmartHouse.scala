package new_benchmarks.complex_smart_house

import join_actors.actor.Actor
import join_actors.api.*
import join_patterns.util.*
import new_benchmarks.mixin.MessageFeedBenchmark
import new_benchmarks.{Benchmark, BenchmarkFactory}
import new_benchmarks.simple_smart_house.*
import new_benchmarks.complex_smart_house.ComplexSmartHouse.*
import new_benchmarks.*
import new_benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet

import java.util.Date
import scala.collection.immutable.ArraySeq
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.Random

class ComplexSmartHouse(private val algorithm: MatchingAlgorithm, private val config: Config) extends MessageFeedBenchmark[Action]:
  override def prepare(n: Int): MessageFeedTriplet[Action] =
    val allMsgsForNRndMsgs =
      Vector.fill(config.matches)(smartHouseMsgs(n)(GenerateActions.genActionsOfSizeN))

    val msgs = allMsgsForNRndMsgs.flatten :+ ShutOff()

    val (result, ref) = getSmartHouseActor(algorithm).start()

    (result, ref, msgs.to(ArraySeq))

  private def getSmartHouseActor(algorithm: MatchingAlgorithm): Actor[Action, (Long, Int)] =
    var lastNotification = Date(0L)
    var lastMotionInBathroom = Date(0L)
    var actions = 0

    def isSorted: Seq[Date] => Boolean = times =>
      times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }

    inline def bathroomOccupied(
      inline times: Seq[Date],
      inline rooms: Seq[String],
      inline mStatus: Boolean,
      inline lStatus: Boolean,
      inline value: Int
    ) = isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

    inline def occupiedHome(
      inline times: Seq[Date],
      inline statuses: Seq[Boolean],
      inline mRoom0: String,
      inline mRoom1: String,
      inline cRoom: String
    ) =
      isSorted(times) && statuses.forall(
        _ == true
      ) && mRoom0 == "front_door" && cRoom == "front_door" && mRoom1 == "entrance_hall"

    inline def emptyHome(
      inline times: Seq[Date],
      inline statuses: Seq[Boolean],
      inline mRoom0: String,
      inline mRoom1: String,
      inline cRoom: String
    ) =
      isSorted(times) && statuses.forall(
        _ == true
      ) && mRoom0 == "entrance_hall" && cRoom == "front_door" && mRoom1 == "front_door"

    Actor[Action, (Long, Int)] {
      receive { (selfRef: ActorRef[Action]) =>
        // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
      {
        case (
          Motion(_: Int, mStatus: Boolean, mRoom: String, t0: Date),
          AmbientLight(_: Int, value: Int, alRoom: String, t1: Date),
          Light(_: Int, lStatus: Boolean, lRoom: String, t2: Date)
        ) if bathroomOccupied(
            List(t0, t1, t2),
            List(mRoom, lRoom, alRoom),
            mStatus,
            lStatus,
            value
        ) =>

          lastNotification = Date()
          lastMotionInBathroom = lastNotification
          actions += 1
          Continue

        // E5. Detect home arrival or leaving based on a particular sequence of messages, and activate the corresponding scene.
        case (
          Motion(_: Int, mStatus0: Boolean, mRoom0: String, t0: Date),
          Contact(_: Int, cStatus: Boolean, cRoom: String, t1: Date),
          Motion(_: Int, mStatus1: Boolean, mRoom1: String, t2: Date)
        ) if occupiedHome(
          List(t0, t1, t2),
          List(mStatus0, mStatus1, cStatus),
          mRoom0,
          mRoom1,
          cRoom
        ) =>
          lastNotification = Date()
          actions += 1
          Continue

        case (
          Motion(_: Int, mStatus0: Boolean, mRoom0: String, t0: Date),
          Contact(_: Int, cStatus: Boolean, cRoom: String, t1: Date),
          Motion(_: Int, mStatus1: Boolean, mRoom1: String, t2: Date)
        ) if emptyHome(
          List(t0, t1, t2),
          List(mStatus0, mStatus1, cStatus),
          mRoom0,
          mRoom1,
          cRoom
        ) =>
          lastNotification = Date()
          actions += 1
          Continue

        case ShutOff() =>
          Stop((System.currentTimeMillis(), actions))
      }
      }(algorithm)
    }

  private def smartHouseMsgs(n: Int)(generator: Int => Vector[Action]): Vector[Action] =
    val randomMsgs = generator(n)

    val correctMsgs =
      Random.nextInt(3) match
        case 0 =>
          // List of messages that trigger E1
          Vector(
            Motion(0, true, "bathroom"),
            AmbientLight(0, 30, "bathroom"),
            Light(0, false, "bathroom")
          )
        case 1 =>
          // List of messages that trigger E5.1
          Vector(
            Motion(0, true, "front_door"),
            Contact(0, true, "front_door"),
            Motion(0, true, "entrance_hall")
          )
        case 2 =>
          // List of messages that trigger E5.2
          Vector(
            Motion(0, true, "entrance_hall"),
            Contact(0, true, "front_door"),
            Motion(0, true, "front_door")
          )

    intercalateCorrectMsgs(correctMsgs, randomMsgs)

object ComplexSmartHouse extends BenchmarkFactory:
  override def apply(algorithm: MatchingAlgorithm, config: ComplexSmartHouseConfig): InstanceType = new ComplexSmartHouse(algorithm, config)
  
  override type Config = ComplexSmartHouseConfig
  override type PassPrereqs = MessageFeedTriplet[Action]
  override type InstanceType = ComplexSmartHouse


