package new_benchmarks

import join_patterns.matching.MatchingAlgorithm

private def standardDeviation(values: Seq[Long], average: Long): Double =
  Math.sqrt(values.map(v => Math.pow(v - average, 2)).sum / (values.size - 1))

final case class ProcessedRepetitions(data: Seq[Long], average: Long, std: Option[Double])
def processRepetitions(repetitions: Repetitions): ProcessedRepetitions =
  val millis = repetitions.map(_.toMillis)

  val averageMillis = millis.sum / millis.size

  if millis.size > 1 then ProcessedRepetitions(millis, averageMillis, Some(standardDeviation(millis, averageMillis)))
  else ProcessedRepetitions(millis, averageMillis, None)

type ProcessedBenchmarkResults = Seq[ProcessedRepetitions]
def processBenchmarkResults(results: BenchmarkResults): ProcessedBenchmarkResults =
  results.map(processRepetitions)  

type ProcessedBenchmarkSeriesResults = Seq[(MatchingAlgorithm, ProcessedBenchmarkResults)]
def processBenchmarkSeriesResults(results: BenchmarkSeriesResults): ProcessedBenchmarkSeriesResults =
  results.map((algo, data) => (algo, processBenchmarkResults(data)))
