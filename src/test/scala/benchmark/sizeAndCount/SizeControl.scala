package test.benchmark.sizeCount

import test.classes.sizeCount.Size1Count1
import test.classes.sizeCount.sizes._
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}
import test.classes.sizeCount.{A, B, C, D, E, F, G, H, I, J}

@main
def sizeControlBenchmark =
  val maxHits = 2000

  Benchmark(
    "Size Control",
    10,
    100,
    BenchmarkPass(
      "Size1",
      () => {
        val actor  = Size1Count1(maxHits)
        val result = actor.run_without_macro

        for _ <- 0 to maxHits do actor.ref.send(A())

        result
      }
    ),
    List(
      BenchmarkPass(
        "Size2",
        () => {
          val actor  = Size2(maxHits)
          val result = actor.run_without_macro

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())

          result
        }
      ),
      BenchmarkPass(
        "Size3",
        () => {
          val actor  = Size3(maxHits)
          val result = actor.run_without_macro

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())

          result
        }
      ),
      BenchmarkPass(
        "Size4",
        () => {
          val actor  = Size4(maxHits)
          val result = actor.run_without_macro

          for _ <- 0 to maxHits do
            actor.ref.send(A())
            actor.ref.send(B())
            actor.ref.send(C())
            actor.ref.send(D())

          result
        }
      ),
      BenchmarkPass(
        "Size5",
        () => {
          val actor  = Size5(maxHits)
          val result = actor.run_without_macro

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
        "Size6",
        () => {
          val actor  = Size6(maxHits)
          val result = actor.run_without_macro

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
        "Size7",
        () => {
          val actor  = Size7(maxHits)
          val result = actor.run_without_macro

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
        "Size8",
        () => {
          val actor  = Size8(maxHits)
          val result = actor.run_without_macro

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
        "Size9",
        () => {
          val actor  = Size9(maxHits)
          val result = actor.run_without_macro

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
        "Size10",
        () => {
          val actor  = Size10(maxHits)
          val result = actor.run_without_macro

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
