package benchmarks.size

import join_actors.api.*
import benchmarks.BenchmarkFactory
import benchmarks.mixin.MessageFeedBenchmark
import benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet
import benchmarks.size.SizeConfig
import benchmarks.size.Size.*
import benchmarks.size.SizeMsg.Terminate

import scala.collection.immutable.ArraySeq

class Size(private val matcher: MatcherFactory, private val config: Config)
    extends MessageFeedBenchmark[SizeMsg]:
  override def prepare(patternSize: Int): MessageFeedTriplet[SizeMsg] =
    val (_, actorGen, msgGen) =
      if config.noise then sizeBenchmarksWithNoise(patternSize)(config.numberOfNoiseMsgs)
      else sizeBenchmarks(patternSize)

    val actor = actorGen(matcher)
    val msgs = msgGen(config.matches) :+ Terminate()

    val (result, ref) = actor.start()

    (result, ref, msgs.to(ArraySeq))

object Size extends BenchmarkFactory:
  override def apply(matcher: MatcherFactory, config: Config): Size = new Size(matcher, config)

  override type Config = SizeConfig
  override type PassPrereqs = MessageFeedTriplet[SizeMsg]
  override type InstanceType = Size
