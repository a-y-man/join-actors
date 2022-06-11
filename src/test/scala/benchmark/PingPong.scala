package test.benchmark.pingPong

import test.classes.pingPong.{Pinger, Ponger}
import test.classes.Msg
import test.benchmark.Benchmark

def setup(maxHits: Int): (Pinger, Ponger) =
  val ping = Pinger(maxHits)
  val pong = Ponger(maxHits)

  ping.pongRef = Some(pong.ref)
  pong.pingRef = Some(ping.ref)

  (ping, pong)

val maxHits = 100_000

@main
def pingPongBenchmark =
  println("=" * 80)
  Benchmark(
    "Ping Pong Macro",
    () => {
      val (ping, pong) = setup(maxHits)
      pong.run_as_future
      ping.run_as_future
    },
    5,
    100
  ).run

  println("=" * 80)
  Benchmark(
    "Ping Pong Macro Unyielded",
    () => {
      val (ping, pong) = setup(maxHits)
      pong.run_as_future_unyielded
      ping.run_as_future_unyielded
    },
    5,
    100
  ).run

  println("=" * 80)
  Benchmark(
    "Ping Pong Base",
    () => {
      val (ping, pong) = setup(maxHits)
      pong.run_without_macro
      ping.run_without_macro
    },
    5,
    100
  ).run

  println("=" * 80)
  Benchmark(
    "Ping Pong Base Unyielded",
    () => {
      val (ping, pong) = setup(maxHits)
      pong.run_without_macro_unyielded
      ping.run_without_macro_unyielded
    },
    5,
    100
  ).run
