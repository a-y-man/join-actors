package test.benchmark.sizeCount

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import test.ALGORITHM
import test.benchmark.Benchmark
import test.benchmark.BenchmarkPass
import test.classes.Msg
import test.classes.sizeCount.A
import test.classes.sizeCount.B
import test.classes.sizeCount.C
import test.classes.sizeCount.D
import test.classes.sizeCount.E
import test.classes.sizeCount.F
import test.classes.sizeCount.G
import test.classes.sizeCount.H
import test.classes.sizeCount.I
import test.classes.sizeCount.J
import test.classes.sizeCount.Size1Count1
import test.classes.sizeCount.sizesWithNoise._

import scala.util.Random
object GenerateTestMsgs {
  def genMsgsOfSizeN(n: Int): List[Msg] =
    val pickMsg = Gen.oneOf(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
    Gen.containerOfN[List, Msg](n, pickMsg).sample.get
}

@main
def sizeWithNoiseBenchmark =
  val maxHits = 2000

  Benchmark(
    "Size",
    10,
    10,
    BenchmarkPass(
      s"Size1 using ${ALGORITHM.toString()}",
      () => {
        val actor  = Size1Noise(maxHits)
        val result = actor.run_as_future

        for _ <- 0 to maxHits do
          Random.nextInt(2) match
            case 0 => actor.ref.send(A())
            case 1 =>
              val msgs = GenerateTestMsgs.genMsgsOfSizeN(1).appended(A())
              msgs.foreach(actor.ref.send(_))
        result
      }
    ),
    List(
      BenchmarkPass(
        s"Size2 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size2Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs = GenerateTestMsgs.genMsgsOfSizeN(2).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))
          result
        }
      ),
      BenchmarkPass(
        s"Size3 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size3Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))

              case 1 =>
                val msgs = GenerateTestMsgs.genMsgsOfSizeN(3).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))
          result
        }
      ),
      BenchmarkPass(
        s"Size4 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size4Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs = GenerateTestMsgs.genMsgsOfSizeN(4).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))

          result
        }
      ),
      BenchmarkPass(
        s"Size5 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size5Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs =
                  GenerateTestMsgs.genMsgsOfSizeN(5).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))

          result
        }
      ),
      BenchmarkPass(
        s"Size6 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size6Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs =
                  GenerateTestMsgs.genMsgsOfSizeN(6).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))

          result
        }
      ),
      BenchmarkPass(
        s"Size7 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size7Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G())
          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs = GenerateTestMsgs
                  .genMsgsOfSizeN(7)
                  .appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))

          result
        }
      ),
      BenchmarkPass(
        s"Size8 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size8Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G(), H())
          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs = GenerateTestMsgs.genMsgsOfSizeN(8).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))
          result
        }
      ),
      BenchmarkPass(
        s"Size9 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size9Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G(), H(), I())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs = GenerateTestMsgs.genMsgsOfSizeN(9).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))
          result
        }
      ),
      BenchmarkPass(
        s"Size10 using ${ALGORITHM.toString()}",
        () => {
          val actor       = Size10Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())

          for _ <- 0 to maxHits do
            Random.nextInt(2) match
              case 0 =>
                correctMsgs.foreach(actor.ref.send(_))
              case 1 =>
                val msgs = GenerateTestMsgs.genMsgsOfSizeN(10).appendedAll(correctMsgs)
                msgs.foreach(actor.ref.send(_))

          result
        }
      )
    )
  ).run
