package test.benchmark.pingPong

import test.classes.pingPong.{Pinger, Ponger}
import test.classes.Msg
import java.util.concurrent.TimeUnit

def setup(maxHits: Int): (Thread, Thread) =
  println("start setup")
  val ping       = Pinger(maxHits)
  val pong       = Ponger(maxHits)
  val pingThread = Thread(ping)
  val pongThread = Thread(pong)

  ping.pongRef = Some(pong.ref)
  pong.pingRef = Some(ping.ref)
  println("end setup")

  (pingThread, pongThread)

def warmup(warmupIterations: Int, maxHits: Int) =
  println("start warmup")

  for _ <- 0 to warmupIterations do
    val (pingThread, pongThread) = setup(maxHits)
    pingThread.start
    pongThread.start

    pingThread.join
    pongThread.join

  println("end warmup")

def benchmark(iterations: Int, maxHits: Int): (Long, Long) =
  println("start benchmark")
  val start = System.nanoTime

  for _ <- 0 to iterations do
    val (pingThread, pongThread) = setup(maxHits) // should it be counted in the benchmark ?
    pingThread.start
    pongThread.start

    pingThread.join
    pongThread.join

  val end = System.nanoTime
  println("end benchmark")

  (start, end)

@main
def pingPongBenchmark =
  val iterations: Int       = 100
  val warmupIterations: Int = 5
  val maxHits               = 100_000

  println(f"Ping Pong benchmark iterations: $iterations, warmup iterations: $warmupIterations")

  println("Ping Pong benchmark start")
  warmup(warmupIterations, maxHits)
  val (start, end) = benchmark(iterations, maxHits)
  println("Ping Pong benchmark end")

  val elapsed         = end - start
  val inSeconds: Long = TimeUnit.SECONDS.convert(elapsed, TimeUnit.NANOSECONDS);
  println(f"Time elapsed : $elapsed ns / $inSeconds s")
  println(
    f"Average time per iteration : ${elapsed / iterations} ns / ${inSeconds.toDouble / iterations} s"
  )
