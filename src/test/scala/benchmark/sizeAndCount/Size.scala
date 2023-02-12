package test.benchmark.sizeCount

import test.classes.sizeCount.Size1Count1
import test.classes.sizeCount.sizes._
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}
import test.classes.sizeCount.{A, B, C, D, E, F, G, H, I, J}
import test.ALGORITHM
@main
def sizeBenchmark =
  val maxHits = 2000

  Benchmark(
    "Size",
    10,
    100,
    BenchmarkPass(
      s"Size1 using ${ALGORITHM.toString()}",
      () => {
        val actor  = Size1Count1(maxHits)
        val result = actor.run_as_future

        for _ <- 0 to maxHits do actor.ref.send(A())

        result
      }
    ),
    List(
      BenchmarkPass(
        s"Size2 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size2(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())

          result
        }
      ),
      BenchmarkPass(
        s"Size3 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size3(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())

          result
        }
      ),
      BenchmarkPass(
        s"Size4 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size4(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())

          result
        }
      ),
      BenchmarkPass(
        s"Size5 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size5(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())
            actor.ref.send(E())

          result
        }
      ),
      BenchmarkPass(
        s"Size6 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size6(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())
            actor.ref.send(E())
            actor.ref.send(F())

          result
        }
      ),
      BenchmarkPass(
        s"Size7 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size7(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())
            actor.ref.send(E())
            actor.ref.send(F())
            actor.ref.send(G())

          result
        }
      ),
      BenchmarkPass(
        s"Size8 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size8(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())
            actor.ref.send(E())
            actor.ref.send(F())
            actor.ref.send(G())
            actor.ref.send(H())

          result
        }
      ),
      BenchmarkPass(
        s"Size9 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size9(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())
            actor.ref.send(E())
            actor.ref.send(F())
            actor.ref.send(G())
            actor.ref.send(H())
            actor.ref.send(I())

          result
        }
      ),
      BenchmarkPass(
        s"Size10 using ${ALGORITHM.toString()}",
        () => {
          val actor  = Size10(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())
            actor.ref.send(E())
            actor.ref.send(F())
            actor.ref.send(G())
            actor.ref.send(H())
            actor.ref.send(I())
            actor.ref.send(J())

          result
        }
      )
    )
  ).run
