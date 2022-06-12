package test.benchmark.pingPong

import test.classes.pingPong.{Pinger, Ponger}
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}

def setup(maxHits: Int): (Pinger, Ponger) =
  val ping = Pinger(maxHits)
  val pong = Ponger(maxHits)

  ping.pongRef = Some(pong.ref)
  pong.pingRef = Some(ping.ref)

  (ping, pong)

val maxHits = 100_000

@main
def pingPongBenchmark =
  Benchmark(
    "Ping Pong",
    10,
    150,
    List(
      BenchmarkPass(
        "Macro",
        () => {
          val (ping, pong) = setup(maxHits)
          pong.run_as_future
          ping.run_as_future
        }
      ),
      BenchmarkPass(
        "Base",
        () => {
          val (ping, pong) = setup(maxHits)
          pong.run_without_macro
          ping.run_without_macro
        }
      )
    )
  ).run
