package new_benchmarks

import join_patterns.matching.MatchingAlgorithm

trait BenchmarkFactory:
  type Config
  type PassPrereqs
  type InstanceType <: Benchmark[PassPrereqs]

  def apply(algorithm: MatchingAlgorithm, config: Config): InstanceType
