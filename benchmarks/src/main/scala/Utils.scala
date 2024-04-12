package benchmarks
import org.scalacheck.*

import scala.util.*

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

object GenerateActions:
  // Set seed for the random generator
  Random.setSeed(512)

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

object GenerateGuardedSizeMsgs:
  import GuardedSizeMsg.*
  private val genAA: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x)).asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABB: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x)).asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCC: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x)).asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDD: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x)).asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDDEE: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x), E(x)).asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDDEEFF: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x), E(x), F(x)).asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDDEEFFGG: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x), E(x), F(x), G(x))
      .asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDDEEFFGGHH: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x), E(x), F(x), G(x), H(x))
      .asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDDEEFFGGHHII: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x), E(x), F(x), G(x), H(x), I(x))
      .asInstanceOf[Vector[GuardedSizeMsg]]

  private val genAABBCCDDEEFFGGHHIIJJ: Gen[Vector[GuardedSizeMsg]] =
    for x <- Gen.choose(0, 100)
    yield Vector(A(x), B(x), C(x), D(x), E(x), F(x), G(x), H(x), I(x), J(x))
      .asInstanceOf[Vector[GuardedSizeMsg]]

  def genGuardedSizeMsgsOfSizeN(n: Int): Option[Vector[GuardedSizeMsg]] =
    n match
      case 1  => genAA.sample
      case 2  => genAABB.sample
      case 3  => genAABBCC.sample
      case 4  => genAABBCCDD.sample
      case 5  => genAABBCCDDEE.sample
      case 6  => genAABBCCDDEEFF.sample
      case 7  => genAABBCCDDEEFFGG.sample
      case 8  => genAABBCCDDEEFFGGHH.sample
      case 9  => genAABBCCDDEEFFGGHHII.sample
      case 10 => genAABBCCDDEEFFGGHHIIJJ.sample
      case _  => None

def genNMatchingMsgSeqs[A](patSize: Int)(generator: Int => Seq[A])(nMatches: Int)(
    isShuffled: Boolean
) =
  val nMatchingMsgsSeqs = LazyList.fill(nMatches)(generator(patSize)).flatten
  if isShuffled then Random.shuffle(nMatchingMsgsSeqs)
  else nMatchingMsgsSeqs
