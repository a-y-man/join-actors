package benchmarks

import join_actors.api.*

private def standardDeviation(values: Seq[Long], average: Double): Double =
  Math.sqrt(values.map(v => Math.pow(v - average, 2)).sum / (values.size - 1))

final case class ProcessedRepetitions(
    data: Seq[Long],
    average: Double,
    std: Option[Double],
    matches: Option[Seq[Int]],
    homogeneousMatches: Option[Int]
)

def processRepetitions(repetitions: Repetitions): ProcessedRepetitions =
  val nanos = repetitions.map(_.duration.toNanos)

  val averageNanos = nanos.sum.toDouble / nanos.size

  val matchesOpt =
    val matchesPerRun = repetitions.map(_.matches)
    if matchesPerRun.forall(_.isDefined) then
      Some(matchesPerRun.collect { case Some(value) => value })
    else None

  val homogeneousMatches = matchesOpt.flatMap: seq =>
    if seq.distinct.size == 1 then Some(seq.head) else None

  if nanos.size > 1 then
    ProcessedRepetitions(
      nanos,
      averageNanos,
      Some(standardDeviation(nanos, averageNanos)),
      matchesOpt,
      homogeneousMatches
    )
  else ProcessedRepetitions(nanos, averageNanos, None, matchesOpt, homogeneousMatches)

type ProcessedBenchmarkResults = Seq[ProcessedRepetitions]
def processBenchmarkResults(results: BenchmarkResults): ProcessedBenchmarkResults =
  results.map(processRepetitions)

type ProcessedBenchmarkSeriesResults = Seq[(MatcherFactory, ProcessedBenchmarkResults)]
def processBenchmarkSeriesResults(
    results: BenchmarkSeriesResults
): ProcessedBenchmarkSeriesResults =
  results.map((matcher, data) => (matcher, processBenchmarkResults(data)))
