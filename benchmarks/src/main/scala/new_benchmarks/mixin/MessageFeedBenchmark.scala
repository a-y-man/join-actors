package new_benchmarks.mixin

import join_actors.api.*
import new_benchmarks.Benchmark
import join_patterns.util.*

import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait MessageFeedBenchmark[MessageType] extends Benchmark[(Future[?], ActorRef[MessageType], ArraySeq[MessageType])]:
  override def run(passConfig: (Future[?], ActorRef[MessageType], ArraySeq[MessageType])): Unit =
    val (result, ref, msgs) = passConfig

    for msg <- msgs.fast do
      ref ! msg

    Await.result(result, Duration.Inf)
