package test.benchmark.sizeCount

import test.classes.sizeCount.counts._
import test.classes.sizeCount.Size1Count1
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}
import test.classes.sizeCount.{A, B, C, D, E, F, G, H, I, J}
import scala.util.Random

@main
def countBenchmark =
  val maxHits = 3_000

  Benchmark(
    "Count",
    10,
    100,
    BenchmarkPass(
      "Count1",
      () => {
        val actor  = Size1Count1(maxHits)
        val result = actor.run_as_future

        for _ <- 0 to maxHits do actor.ref.send(A())

        result
      }
    ),
    List(
      BenchmarkPass(
        "Count2",
        () => {
          val actor  = Count2(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(2) match
                case 0 => A()
                case 1 => B()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count3",
        () => {
          val actor  = Count3(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(3) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count4",
        () => {
          val actor  = Count4(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(4) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count5",
        () => {
          val actor  = Count5(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(5) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
                case 4 => E()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count6",
        () => {
          val actor  = Count6(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(6) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
                case 4 => E()
                case 5 => F()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count7",
        () => {
          val actor  = Count7(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(7) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
                case 4 => E()
                case 5 => F()
                case 6 => G()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count8",
        () => {
          val actor  = Count8(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(8) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
                case 4 => E()
                case 5 => F()
                case 6 => G()
                case 7 => H()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count9",
        () => {
          val actor  = Count9(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(9) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
                case 4 => E()
                case 5 => F()
                case 6 => G()
                case 7 => H()
                case 8 => I()
            }

          result
        }
      ),
      BenchmarkPass(
        "Count10",
        () => {
          val actor  = Count10(maxHits)
          val result = actor.run_as_future

          for _ <- 0 to maxHits do
            actor.ref.send {
              Random.nextInt(10) match
                case 0 => A()
                case 1 => B()
                case 2 => C()
                case 3 => D()
                case 4 => E()
                case 5 => F()
                case 6 => G()
                case 7 => H()
                case 8 => I()
                case 9 => J()
            }

          result
        }
      )
    )
  ).run
