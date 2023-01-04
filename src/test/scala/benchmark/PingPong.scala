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

@main
def pingPongBenchmark =
  val maxHits = 100

  Benchmark(
    "Ping Pong",
    10,
    200,
    BenchmarkPass(
      "Control",
      () => {
        val (ping, pong) = setup(maxHits)
        pong.run_without_macro
        ping.run_without_macro
      }
    ),
    List(
      BenchmarkPass(
        "Macro",
        () => {
          val (ping, pong) = setup(maxHits)
          pong.run_as_future
          ping.run_as_future
        }
      )
    )
  ).run
