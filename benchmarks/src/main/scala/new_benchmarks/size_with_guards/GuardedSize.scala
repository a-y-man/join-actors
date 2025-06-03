package new_benchmarks.size_with_guards

import join_patterns.matching.MatchingAlgorithm
import new_benchmarks.mixin.MessageFeedBenchmark
import new_benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet
import new_benchmarks.{BenchmarkFactory, size_with_guards}
import new_benchmarks.size_with_guards.GuardedSizeMsg.Terminate
import new_benchmarks.size_with_guards.GuardedSizeVariant.*
import new_benchmarks.size_with_guards.GuardedSize.*

import scala.collection.immutable.ArraySeq

class GuardedSize(private val algorithm: MatchingAlgorithm, private val config: Config) extends MessageFeedBenchmark[GuardedSizeMsg]:
  override def prepare(param: Int): MessageFeedTriplet[GuardedSizeMsg] =
    val (_, actorGen, msgGen) =
      config.variant match
        case Normal => sizeBenchmarkWithPayloadData(param - 1)
        case Noisy => sizeBenchmarkWithNoisyPayloadData(param - 1)
        case NonMatchingPayloads => sizeBenchmarkWithNonMatchingPayloadData(param - 1)

    val actor = actorGen(algorithm)
    val msgs = msgGen(config.matches) :+ Terminate()

    val (result, ref) = actor.start()

    (result, ref, msgs.to(ArraySeq))

object GuardedSize extends BenchmarkFactory:
  override def apply(algorithm: MatchingAlgorithm, config: Config): GuardedSize = new GuardedSize(algorithm, config)

  override type Config = GuardedSizeConfig
  override type PassPrereqs = MessageFeedTriplet[GuardedSizeMsg]
  override type InstanceType = GuardedSize
