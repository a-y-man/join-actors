package benchmarks.size_with_guards

import join_actors.api.*
import benchmarks.mixin.MessageFeedBenchmark
import benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet
import benchmarks.BenchmarkFactory
import benchmarks.size_with_guards
import benchmarks.size_with_guards.GuardedSizeMsg.Terminate
import benchmarks.size_with_guards.GuardedSizeVariant.*
import benchmarks.size_with_guards.GuardedSize.*

import scala.collection.immutable.ArraySeq

class GuardedSize(private val matcher: MatcherFactory, private val config: Config)
    extends MessageFeedBenchmark[GuardedSizeMsg]:
  override def prepare(patternSize: Int): MessageFeedTriplet[GuardedSizeMsg] =
    val (_, actorGen, msgGen) =
      config.variant match
        case Normal => sizeBenchmarkWithPayloadData(patternSize)
        case Noisy => sizeBenchmarkWithNoisyPayloadData(patternSize)(config.numberOfNoiseMsgs.get)
        case NonMatchingPayloads => sizeBenchmarkWithNonMatchingPayloadData(patternSize)(config.nonMatchingPayload.get)

    val actor = actorGen(matcher)
    val msgs = msgGen(config.matches) :+ Terminate()

    val (result, ref) = actor.start()

    (result, ref, msgs.to(ArraySeq))

object GuardedSize extends BenchmarkFactory:
  override def apply(matcher: MatcherFactory, config: Config): GuardedSize =
    new GuardedSize(matcher, config)

  override type Config = GuardedSizeConfig
  override type PassPrereqs = MessageFeedTriplet[GuardedSizeMsg]
  override type InstanceType = GuardedSize
