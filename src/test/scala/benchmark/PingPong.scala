package test.benchmark.pingPong

import test.classes.pingPong.{Pinger, Ponger}
import test.classes.Msg
import test.benchmark.{Benchmark, BenchmarkPass}
import test.ALGORITHM

def setup(maxHits: Int): (Pinger, Ponger) =
  val ping = Pinger(maxHits)
  val pong = Ponger(maxHits)

  ping.pongRef = Some(pong.ref)
  pong.pingRef = Some(ping.ref)

  (ping, pong)

@main
def pingPongBenchmark =
  val maxHits = 100_000
  Benchmark(
    "Ping Pong",
    10,
    200,
    BenchmarkPass(
      "Control",
      () => {
        val (ping, pong) = setup(maxHits)
        pong.run_as_future
        ping.run_as_future
      }
    ),
    List(
      BenchmarkPass(
        s"Macro using ${ALGORITHM.toString()}",
        () => {
          val (ping, pong) = setup(maxHits)
          pong.run_as_future
          ping.run_as_future
        }
      )
    )
  ).run
