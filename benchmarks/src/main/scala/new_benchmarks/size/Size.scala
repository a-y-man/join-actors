package new_benchmarks.size

import join_patterns.matching.MatchingAlgorithm
import new_benchmarks.BenchmarkFactory
import new_benchmarks.mixin.MessageFeedBenchmark
import new_benchmarks.mixin.MessageFeedBenchmark.MessageFeedTriplet
import new_benchmarks.size.SizeConfig
import new_benchmarks.size.Size.*
import new_benchmarks.size.SizeMsg.Terminate

import scala.collection.immutable.ArraySeq

class Size(private val algorithm: MatchingAlgorithm, private val config: Config) extends MessageFeedBenchmark[SizeMsg]:
  override def prepare(param: Int): MessageFeedTriplet[SizeMsg] =
    val (_, actorGen, msgGen) =
      if config.noise then sizeBenchmarksWithNoise(param - 1)
      else sizeBenchmarks(param - 1)

    val actor = actorGen(algorithm)
    val msgs = msgGen(config.matches) :+ Terminate()

    val (result, ref) = actor.start()

    (result, ref, msgs.to(ArraySeq))

object Size extends BenchmarkFactory:
  override def apply(algorithm: MatchingAlgorithm, config: Config): Size = new Size(algorithm, config)

  override type Config = SizeConfig
  override type PassPrereqs = MessageFeedTriplet[SizeMsg]
  override type InstanceType = Size
