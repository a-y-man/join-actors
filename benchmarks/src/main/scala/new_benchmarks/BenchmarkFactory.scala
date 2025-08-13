package new_benchmarks

import join_actors.api.*

trait BenchmarkFactory:
  type Config
  type PassPrereqs
  type InstanceType <: Benchmark[PassPrereqs]

  def apply(matcher: MatcherFactory, config: Config): InstanceType
