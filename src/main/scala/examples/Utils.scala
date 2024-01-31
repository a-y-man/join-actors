package examples

import scala.util.*
import org.scalacheck.Gen

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
