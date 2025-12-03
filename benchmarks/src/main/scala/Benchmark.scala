package benchmarks

trait Benchmark[PassPrereqs]:
  def prepare(param: Int): PassPrereqs

  def run(passConfig: PassPrereqs): Unit
