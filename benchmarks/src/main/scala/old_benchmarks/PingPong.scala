package old_benchmarks

import join_actors.api.*
import os.Path

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

type Ponger = ActorRef[Ping | Done]
type Pinger = ActorRef[Pong | Done]

sealed trait PingPong
case class Ping(ref: Pinger, hits: Int) extends PingPong
case class Pong(ref: Ponger, hits: Int) extends PingPong
case class Done(hits: Int) extends PingPong

def pingPonger(maxHits: Int = 100, matcher: MatcherFactory) =
  val pingActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (pingRef: Pinger) =>
        {
          case Pong(pongRef, x) =>
            if x < maxHits then
              pongRef ! Ping(pingRef, x + 1)
              Continue
            else
              pongRef ! Done(x)
              pingRef ! Done(x)
              Continue
          case Done(x) =>
            Stop(x)
        }
      }(matcher)
    }

  val pongActor: Actor[PingPong, Int] =
    Actor[PingPong, Int] {
      receive { (pongRef: Ponger) =>
        {
          case Ping(pingRef, x) =>
            if x < maxHits then
              pingRef ! Pong(pongRef, x + 1)
              Continue
            else
              pingRef ! Done(x)
              pongRef ! Done(x)
              Continue
          case Done(x) =>
            Stop(x)
        }
      }(matcher)
    }

  (pingActor, pongActor)

def measurePingPong(maxHits: Int, matcher: MatcherFactory) =

  val (pingActor, pongActor) = pingPonger(maxHits, matcher)
  val (result1, pinger) = pingActor.start()
  val (result2, ponger) = pongActor.start()

  Future {
    val startTime = System.currentTimeMillis()

    val results = Future.sequence(Seq(result1, result2))

    ponger ! Ping(pinger, 0)

    Await.ready(results, Duration(30, TimeUnit.SECONDS))

    val endTime = System.currentTimeMillis()
    Measurement(endTime - startTime, maxHits)
  }

def pingPongBenchmark(maxHits: Int, matcher: MatcherFactory) =
  Benchmark(
    "Ping Pong",
    matcher,
    5,
    10,
    BenchmarkPass(
      "Control Null Pass",
      () => measurePingPong(maxHits, matcher)
    ),
    List(
      BenchmarkPass(
        s"PingPong using ${matcher}",
        () => measurePingPong(maxHits, matcher)
      )
    )
  )

def runPingPongBenchmark(
    maxHits: Int,
    writeToFile: Boolean = false,
    outputDataDir: Path = os.pwd / "benchmarks" / "data"
) =
  val matchers: List[MatcherFactory] =
    List(
      StatefulTreeMatcher,
      MutableStatefulMatcher,
      LazyMutableMatcher,
      WhileLazyMatcher,
      FilteringWhileMatcher,
      WhileEagerMatcher,
      ArrayWhileMatcher,
      EagerParallelMatcher(2),
      EagerParallelMatcher(4),
      EagerParallelMatcher(6),
      EagerParallelMatcher(8),
      LazyParallelMatcher(2),
      LazyParallelMatcher(4),
      LazyParallelMatcher(6),
      LazyParallelMatcher(8),
      FilteringParallelMatcher(2),
      FilteringParallelMatcher(4),
      FilteringParallelMatcher(6),
      FilteringParallelMatcher(8),
      ArrayParallelMatcher(2),
      ArrayParallelMatcher(4),
      ArrayParallelMatcher(6),
      ArrayParallelMatcher(8)
    )


  val measurements = matchers map { matcher =>
    println(
      s"${Console.GREEN}${Console.UNDERLINED}Running benchmark for $matcher${Console.RESET}"
    )
    val measurement = pingPongBenchmark(maxHits, matcher).run()
    println(
      s"${Console.RED}${Console.UNDERLINED}Benchmark for $matcher finished${Console.RESET}"
    )
    (matcher, measurement)
  }

  if writeToFile then saveToFile("PingPong", measurements, outputDataDir)
