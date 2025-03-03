package join_actors.examples

import org.scalacheck.Gen

import scala.util.*

def printResult[A](result: Try[A]): Unit = result match
  case Failure(exception) => println("Failed with: " + exception.getMessage)
  case Success(number)    => println("Succeed with: " + number)

object GenerateRandomMsgs:
  // Set seed for the random generator
  // Random.setSeed(1234567890)

  private val genA: Gen[A] = Gen.const(A())
  private val genB: Gen[B] = Gen.const(B())
  private val genC: Gen[C] = Gen.const(C())
  private val genD: Gen[D] = Gen.choose(0, 100).map(D(_))
  private val genE: Gen[E] = Gen.choose(0, 100).map(E(_))
  private val genF: Gen[F] = Gen.choose(0, 100).map(F(_))
  private val genG: Gen[G] =
    for
      b <- Gen.choose(0, 100)
      a <- Gen.alphaStr
      c <- Gen.choose(0, 100)
      d <- Gen.oneOf(true, false)
    yield G(b, a, c, d)

  private val genMsg: Gen[Msg] = Gen.oneOf(genA, genB, genC, genD, genE, genF, genG)

  def genRandomMsgs(n: Int): List[Msg] =
    Gen.containerOfN[List, Msg](n, genMsg).sample.get

  def genWeightedRandomMsgs(n: Int, weights: List[Tuple2[Int, Gen[Msg]]]): List[Msg] =
    val genMsg = Gen.frequency(weights*)
    Gen.containerOfN[List, Msg](n, genMsg).sample.get

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

/*
This object generates random smart house actions.
 */
object GenerateActions:
  import Action.*
  // Set seed for the random generator
  Random.setSeed(42)

  private val genMotion: Gen[Action] = for
    i <- Gen.choose(0, 100)
    b <- Gen.oneOf(true, false)
    s <- Gen.alphaStr
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

  def genActionsOfSizeNWithMatchingTypes(n: Int): Vector[Action] =
    val pickAction =
      Gen.oneOf(genMotion, genAmbientLight, genLight, genContact)
    Gen.containerOfN[Vector, Action](n, pickAction).sample.get

  def genActionsOfSizeN(n: Int): Vector[Action] =
    val pickAction =
      Gen.oneOf(genMotion, genAmbientLight, genLight, genContact, genConsumption, genHeatingF)
    Gen.containerOfN[Vector, Action](n, pickAction).sample.get
