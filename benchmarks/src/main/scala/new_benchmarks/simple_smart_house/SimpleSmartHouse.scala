package new_benchmarks.simple_smart_house

import join_actors.actor.Actor
import join_actors.api.*
import new_benchmarks.simple_smart_house.SimpleSmartHouse.*
import new_benchmarks.{Benchmark, BenchmarkFactory}

import java.util.Date
import scala.collection.immutable.ArraySeq
import scala.concurrent.Future
import join_patterns.util.*
import new_benchmarks.mixin.MessageFeedBenchmark
import new_benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet

import scala.concurrent.duration.Duration
import scala.concurrent.Await

class SimpleSmartHouse(private val algorithm: MatchingAlgorithm, private val config: Config) extends MessageFeedBenchmark[Action]:
  override def prepare(n: Int): MessageFeedTriplet[Action] =
    val allMsgsForNRndMsgs =
      ArraySeq.fill(config.matches)(simpleSmartHouseMsgsWithPreMatches(n))

    val msgs = allMsgsForNRndMsgs.flatten :+ ShutOff()

    val (result, ref) = getSmartHouseActor(algorithm, config.withHeavyGuard).start()

    (result, ref, msgs)

  private def getSmartHouseActor(algorithm: MatchingAlgorithm, withHeavyGuard: Boolean) =
    var lastNotification = Date(0L)
    var lastMotionInBathroom = Date(0L)
    var actions = 0

    def isSorted: Seq[Date] => Boolean = times =>
      times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }

    def busyLoop(): Unit =
      val start = System.nanoTime()
      while System.nanoTime() - start < 100000 do ()
      ()

    def bathroomOccupied =
      (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
      ) =>
        if withHeavyGuard then busyLoop()
        isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

    Actor[Action, (Long, Int)] {
      receive { (selfRef: ActorRef[Action]) =>
        // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
      {
        case (
          Motion(_: Int, mStatus: Boolean, mRoom: String, t0: Date),
          AmbientLight(_: Int, value: Int, alRoom: String, t1: Date),
          Light(_: Int, lStatus: Boolean, lRoom: String, t2: Date)
          )
          if bathroomOccupied(
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
        case ShutOff() =>
          Stop((System.currentTimeMillis(), actions))
      }
      }(algorithm)
    }

  private def simpleSmartHouseMsgsWithPreMatches(n: Int): ArraySeq[Action] =
    val pat1MatchPrefix = ArraySeq(
      Motion(0, true, "bathroom"),
      AmbientLight(0, 30, "bathroom")
    )
    val pat1MatchSuffix = Light(0, false, "bathroom")

    if n == 0 then pat1MatchPrefix :+ pat1MatchSuffix
    else ArraySeq.fill(n)(pat1MatchPrefix).flatten :+ pat1MatchSuffix
  

object SimpleSmartHouse extends BenchmarkFactory:
  override def apply(algorithm: MatchingAlgorithm, config: SimpleSmartHouseConfig): InstanceType = new SimpleSmartHouse(algorithm, config)
  
  override type Config = SimpleSmartHouseConfig
  override type PassPrereqs = MessageFeedTriplet[Action]
  override type InstanceType = SimpleSmartHouse


