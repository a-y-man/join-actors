package new_benchmarks.mixin

import join_actors.api.*
import new_benchmarks.Benchmark
import join_patterns.util.*
import new_benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet

import scala.collection.immutable.ArraySeq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait MessageFeedBenchmark[MessageType] extends Benchmark[MessageFeedTriplet[MessageType]]:
  override def run(passConfig: MessageFeedTriplet[MessageType]): Unit =
    val (result, ref, msgs) = passConfig

    for msg <- msgs.fast do
      ref ! msg

    Await.result(result, Duration.Inf)

object MessageFeedBenchmark:
  type MessageFeedTriplet[MessageType] = (Future[?], ActorRef[MessageType], ArraySeq[MessageType])
