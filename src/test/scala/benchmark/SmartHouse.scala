package benchmark

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.receive
import org.scalacheck.*
import org.scalatest.run
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass
import test.benchmark.Measurement

import java.util.Date
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.*

sealed trait Action
case class Motion(id: Int, status: Boolean, room: String, timestamp: Date = Date())  extends Action
case class AmbientLight(id: Int, value: Int, room: String, timestamp: Date = Date()) extends Action
case class Light(id: Int, status: Boolean, room: String, timestamp: Date = Date())   extends Action
case class Contact(id: Int, open: Boolean, room: String, timestamp: Date = Date())   extends Action
case class Consumption(meter_id: Int, value: Int, timestamp: Date = Date())          extends Action
case class HeatingF(id: Int, _type: String, timestamp: Date = Date())                extends Action
case class DoorBell(id: Int, timestamp: Date = Date())                               extends Action
case class ShutOff()                                                                 extends Action

object GenerateActions:
  // Set seed for the random generator
  Random.setSeed(512)

  private val genMotion: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr // .suchThat(s => s.length == 10)
  yield Motion(i, b, s).asInstanceOf[Action]

  private val genAmbientLight: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
    s <- Gen.alphaStr
  yield AmbientLight(i, b, s).asInstanceOf[Action]

  private val genLight: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr
  yield Light(i, b, s).asInstanceOf[Action]

  private val genContact: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr
  yield Contact(i, b, s).asInstanceOf[Action]

  private val genConsumption: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.choose(0, 100)
  yield Consumption(i, b).asInstanceOf[Action]

  private val genHeatingF: Gen[Action] = for
    i <- Gen.choose(0, 100)
    s <- Gen.alphaStr
  yield HeatingF(i, s).asInstanceOf[Action]

  private val genDoorBell: Gen[Action] =
    for i <- Gen.choose(0, 100)
    yield DoorBell(i).asInstanceOf[Action]

  def genActionsOfSizeN(n: Int): Option[List[Action]] =
    val pickAction =
      Gen.oneOf(
        genMotion,
        genAmbientLight,
        genLight,
        genContact,
        genConsumption,
        genHeatingF,
        genDoorBell
      )
    Gen.containerOfN[List, Action](n, pickAction).sample

def smartHouseExample(algorithm: MatchingAlgorithm) =
  var lastNotification     = Date(0L)
  var lastMotionInBathroom = Date(0L)
  var actions              = 0
  def isSorted: Seq[Date] => Boolean = times =>
    times.sliding(2).forall { case Seq(x, y) => x.before(y) || x == y }
  def bathroomOccupied =
    (
        times: Seq[Date],
        rooms: Seq[String],
        mStatus: Boolean,
        lStatus: Boolean,
        value: Int
    ) => isSorted(times) && rooms.forall(_ == "bathroom") && mStatus && !lStatus && value <= 40

  def occupiedHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "front_door" && cRoom == "front_door" && mRoom1 == "entrance_hall"

  def emptyHome = (
      times: Seq[Date],
      statuses: Seq[Boolean],
      mRoom0: String,
      mRoom1: String,
      cRoom: String
  ) =>
    isSorted(times) && statuses.forall(
      _ == true
    ) && mRoom0 == "entrance_hall" && cRoom == "front_door" && mRoom1 == "front_door"

  Actor[Action, (Long, Int)] {
    receive { (y: Action, selfRef: ActorRef[Action]) =>
      y match
        // E1. Turn on the lights of the bathroom if someone enters in it, and its ambient light is less than 40 lux.
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
          Next()
        // E5. Detect home arrival or leaving based on a particular sequence of messages, and activate the corresponding scene.
        case (
              Motion(_: Int, mStatus0: Boolean, mRoom0: String, t0: Date),
              Contact(_: Int, cStatus: Boolean, cRoom: String, t1: Date),
              Motion(_: Int, mStatus1: Boolean, mRoom1: String, t2: Date)
            )
            if occupiedHome(
              List(t0, t1, t2),
              List(mStatus0, mStatus1, cStatus),
              mRoom0,
              mRoom1,
              cRoom
            ) =>
          lastNotification = Date()
          actions += 1
          Next()
        case (
              Motion(_: Int, mStatus0: Boolean, mRoom0: String, t0: Date),
              Contact(_: Int, cStatus: Boolean, cRoom: String, t1: Date),
              Motion(_: Int, mStatus1: Boolean, mRoom1: String, t2: Date)
            )
            if emptyHome(
              List(t0, t1, t2),
              List(mStatus0, mStatus1, cStatus),
              mRoom0,
              mRoom1,
              cRoom
            ) =>
          lastNotification = Date()
          actions += 1
          Next()
        case ShutOff() =>
          Stop((System.currentTimeMillis(), actions))
    }(algorithm)
  }

def intercalateCorrectMsgs[A](
    correctMsgs: Vector[A],
    randomMsgs: Vector[A]
): Vector[A] =
  val randomMsgsSize  = randomMsgs.size
  val correctMsgsSize = correctMsgs.size
  if randomMsgsSize >= correctMsgsSize then
    val groupSize = (randomMsgsSize + correctMsgsSize - 1) / correctMsgsSize
    randomMsgs
      .grouped(groupSize) // Chunk the random messages into chunks of size groupSize
      .zipAll(correctMsgs, randomMsgs, randomMsgs.headOption.getOrElse(correctMsgs.last))
      .flatMap { case (randomChunk, correctMsg) => randomChunk :+ correctMsg }
      .toVector
  else randomMsgs ++ correctMsgs

def smartHouseMsgs(n: Int): Vector[Action] =
  val randomMsgs = GenerateActions.genActionsOfSizeN(n).toVector.flatten

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

def measureSmartHouse(
    smartHouseActions: Int,
    msgs: Vector[Action],
    algorithm: MatchingAlgorithm
): Future[Measurement] =
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newVirtualThreadPerTaskExecutor())

  val actor = smartHouseExample(algorithm)

  val (result, actorRef) = actor.start()

  Future {
    val startTime = System.currentTimeMillis()

    for _ <- 1 to smartHouseActions do msgs.foreach(actorRef ! _)

    actorRef ! ShutOff()

    val (endTime, matches) = Await.result(result, Duration.Inf)

    Measurement(endTime - startTime, matches)
  }

def smartHouseBenchmark(
    smartHouseActions: Int,
    rangeOfRandomMsgs: Vector[(Vector[Action], Int)],
    algorithm: MatchingAlgorithm
) =

  val nullPassMsgs = smartHouseMsgs(5)
  Benchmark(
    name = "SmartHouse",
    algorithm = algorithm,
    warmupIterations = 5,
    iterations = 10,
    nullPass = BenchmarkPass(
      s"Null Pass ${algorithm}",
      () => measureSmartHouse(smartHouseActions, nullPassMsgs, algorithm)
    ),
    passes = rangeOfRandomMsgs map { case (msgs, n) =>
      BenchmarkPass(
        s"$n",
        () => measureSmartHouse(smartHouseActions, msgs, algorithm)
      )
    }
  )

@main
def runSmartHouseBenchmark() =
  val statefulTreeAlgorithm = MatchingAlgorithm.StatefulTreeBasedAlgorithm
  val bruteForceAlgorithm   = MatchingAlgorithm.BruteForceAlgorithm

  val smartHouseActions = 5 // Successful matches per benchmark repetition
  val maxRandomMsgs     = 18
  val rndMsgsStep       = 3
  val rangeOfRandomMsgs =
    Vector((0 to maxRandomMsgs by rndMsgsStep)*) map { n =>
      (smartHouseMsgs(n), n)
    }

  List(statefulTreeAlgorithm, bruteForceAlgorithm) foreach { algorithm =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $algorithm${Console.RESET}"
    )
    smartHouseBenchmark(smartHouseActions, rangeOfRandomMsgs, algorithm).run(true)
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $algorithm finished${Console.RESET}"
    )
  }
