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
import test.classes.sizeCount.sizesWithNoise.*

import scala.util.Random
object GenerateTestMsgs:
  def genMsgsOfSizeN(n: Int): List[Msg] =
    val pickMsg = Gen.oneOf(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())
    Gen.containerOfN[List, Msg](n, pickMsg).sample.get

val N                = 10
val warmupIterations = 5
val iterations       = 10

@main
def sizeWithNoiseBenchmark =
  val maxHits = 1000

  Benchmark(
    "Pattern Size",
    warmupIterations,
    iterations,
    BenchmarkPass(
      s"Pattern of size 1 using ${ALGORITHM.toString()} with $N random messages",
      () =>
        val actor  = Size1Noise(maxHits)
        val result = actor.run_as_future

        for _ <- 0 to maxHits do
          val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appended(A())
          msgs.foreach(actor.ref.send(_))
        result
    ),
    List(
      BenchmarkPass(
        s"Pattern of size 2 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size2Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B())

          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))

          result
      ),
      BenchmarkPass(
        s"Pattern of size 3 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size3Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C())

          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))
          result
      ),
      BenchmarkPass(
        s"Pattern of size 4 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size4Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D())

          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))

          result
      ),
      BenchmarkPass(
        s"Pattern of size 5 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size5Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E())

          for _ <- 0 to maxHits do
            val msgs =
              GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))

          result
      ),
      BenchmarkPass(
        s"Pattern of size 6 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size6Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F())

          for _ <- 0 to maxHits do
            val msgs =
              GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))

          result
      ),
      BenchmarkPass(
        s"Pattern of size 7 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size7Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G())
          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs
              .genMsgsOfSizeN(N)
              .appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))

          result
      ),
      BenchmarkPass(
        s"Pattern of size 8 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size8Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G(), H())
          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))
          result
      ),
      BenchmarkPass(
        s"Pattern of size 9 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size9Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G(), H(), I())

          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))
          result
      ),
      BenchmarkPass(
        s"Pattern of size 10 using ${ALGORITHM.toString()} with $N random messages",
        () =>
          val actor       = Size10Noise(maxHits)
          val result      = actor.run_as_future
          val correctMsgs = List(A(), B(), C(), D(), E(), F(), G(), H(), I(), J())

          for _ <- 0 to maxHits do
            val msgs = GenerateTestMsgs.genMsgsOfSizeN(N).appendedAll(correctMsgs)
            msgs.foreach(actor.ref.send(_))

          result
      )
    )
  ).run
