package new_benchmarks.size_with_guards

import GuardedSizeMsg.*
import org.scalacheck.Gen

object GenerateGuardedSizeMsgs:

  private val genA: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield A(x)

  private val genB: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield B(x)

  private val genC: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield C(x)

  private val genD: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield D(x)

  private val genE: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield E(x)

  private val genF: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield F(x)

  private val genG: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield G(x)

  private val genH: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield H(x)

  private val genI: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield I(x)

  private val genJ: Gen[GuardedSizeMsg] =
    for x <- Gen.choose(101, 200)
      yield J(x)

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

  def genNNonMatchingMsgs(patSize: Int)(n: Int): Vector[GuardedSizeMsg] =
    val allGens = Vector(genA, genB, genC, genD, genE, genF, genG, genH, genI, genJ)
    val patMsgs = Gen.oneOf(allGens.take(patSize)).flatMap(identity)
    Gen.containerOfN[Vector, GuardedSizeMsg](n, patMsgs).sample.getOrElse(Vector.empty)

  def genGuardedSizeMsgsOfSizeN(n: Int): Option[Vector[GuardedSizeMsg]] =
    n match
      case 1 => genAA.sample
      case 2 => genAABB.sample
      case 3 => genAABBCC.sample
      case 4 => genAABBCCDD.sample
      case 5 => genAABBCCDDEE.sample
      case 6 => genAABBCCDDEEFF.sample
      case 7 => genAABBCCDDEEFFGG.sample
      case 8 => genAABBCCDDEEFFGGHH.sample
      case 9 => genAABBCCDDEEFFGGHHII.sample
      case 10 => genAABBCCDDEEFFGGHHIIJJ.sample
      case _ => None

