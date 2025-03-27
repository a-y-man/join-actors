package new_benchmarks

import join_patterns.matching.MatchingAlgorithm

final case class ProcessedRepetitions(val data: Seq[Long], val average: Long)
def processRepetitions(repetitions: Repetitions): ProcessedRepetitions =
  val millis = repetitions.map(_.toMillis)
  
  ProcessedRepetitions(millis, millis.sum / millis.size)

type ProcessedBenchmarkResults = Seq[ProcessedRepetitions]
def processBenchmarkResults(results: BenchmarkResults): ProcessedBenchmarkResults =
  results.map(processRepetitions)  

type ProcessedBenchmarkSeriesResults = Seq[(MatchingAlgorithm, ProcessedBenchmarkResults)]
def processBenchmarkSeriesResults(results: BenchmarkSeriesResults): ProcessedBenchmarkSeriesResults =
  results.map((algo, data) => (algo, processBenchmarkResults(data)))
