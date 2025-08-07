package join_patterns.matching

import join_patterns.matching.brute_force.BruteForceMatcher
import join_patterns.matching.immutable.StatefulTreeMatcher
import join_patterns.matching.mutable.MutableStatefulMatcher
import join_patterns.matching.lazy_mutable.LazyMutableMatcher
import join_patterns.matching.while_lazy.WhileLazyMatcher
import join_patterns.matching.filtering_while.FilteringWhileMatcher
import join_patterns.matching.while_eager.WhileEagerMatcher
import join_patterns.matching.eager_parallel.EagerParallelMatcher
import join_patterns.matching.lazy_parallel.LazyParallelMatcher
import join_patterns.matching.filtering_parallel.FilteringParallelMatcher
import join_patterns.matching.array_while.ArrayWhileMatcher
import join_patterns.matching.array_parallel.ArrayParallelMatcher

val runtime = Runtime.getRuntime()
val numCores = runtime.availableProcessors()

enum MatchingAlgorithm:
  case BruteForceAlgorithm
  case StatefulTreeBasedAlgorithm
  case MutableStatefulAlgorithm
  case LazyMutableAlgorithm
  case WhileLazyAlgorithm
  case FilteringWhileAlgorithm
  case WhileEagerAlgorithm
  case EagerParallelAlgorithm(numThreads: Int = numCores)
  case LazyParallelAlgorithm(numThreads: Int = numCores)
  case FilteringParallelAlgorithm(numThreads: Int = numCores)
  case ArrayWhileAlgorithm
  case ArrayParallelAlgorithm(numThreads: Int = numCores)

  override def toString: String =
    this match
      case BruteForceAlgorithm => "BruteForceAlgorithm"
      case StatefulTreeBasedAlgorithm => "StatefulTreeBasedAlgorithm"
      case MutableStatefulAlgorithm => "MutableStatefulAlgorithm"
      case LazyMutableAlgorithm => "LazyMutableAlgorithm"
      case WhileLazyAlgorithm => "WhileLazyAlgorithm"
      case FilteringWhileAlgorithm => "FilteringWhileAlgorithm"
      case WhileEagerAlgorithm => "WhileEagerAlgorithm"
      case EagerParallelAlgorithm(numThreads) => s"EagerParallelAlgorithm_$numThreads"
      case LazyParallelAlgorithm(numThreads) => s"LazyParallelAlgorithm_$numThreads"
      case FilteringParallelAlgorithm(numThreads) => s"FilteringParallelAlgorithm_$numThreads"
      case ArrayWhileAlgorithm => "ArrayWhileAlgorithm"
      case ArrayParallelAlgorithm(numThreads) => s"ArrayParallelAlgorithm_$numThreads"

object MatchingAlgorithm:
  private val cmdStringsAndAlgorithms = List(
    "brute" -> BruteForceAlgorithm,
    "stateful" -> StatefulTreeBasedAlgorithm,
    "mutable" -> MutableStatefulAlgorithm,
    "lazy-mutable" -> LazyMutableAlgorithm,
    "while-lazy" -> WhileLazyAlgorithm,
    "filtering-while" -> FilteringWhileAlgorithm,
    "while-eager" -> WhileEagerAlgorithm,
    "eager-parallel" -> EagerParallelAlgorithm(),
    "lazy-parallel" -> LazyParallelAlgorithm(),
    "filtering-parallel" -> FilteringParallelAlgorithm(),
    "array-while" -> ArrayWhileAlgorithm,
    "array-parallel" -> ArrayParallelAlgorithm()
  )

  private val cmdStringToAlgorithm = cmdStringsAndAlgorithms.toMap

  def parseFromCmdString(algorithmStr: String): Option[MatchingAlgorithm] =
    cmdStringToAlgorithm.get(algorithmStr)

  val CMD_STRINGS: Seq[String] = cmdStringsAndAlgorithms.map(_._1)
  val ALGORITHMS: Seq[MatchingAlgorithm] = cmdStringsAndAlgorithms.map(_._2)


object MatcherSelector:
  private val cmdStringsAndMatcher = List(
    "brute" -> BruteForceMatcher,
    "stateful" -> StatefulTreeMatcher,
    "mutable" -> MutableStatefulMatcher,
    "lazy-mutable" -> LazyMutableMatcher,
    "while-lazy" -> WhileLazyMatcher,
    "filtering-while" -> FilteringWhileMatcher,
    "while-eager" -> WhileEagerMatcher,
    "eager-parallel" -> EagerParallelMatcher,
    "lazy-parallel" -> LazyParallelMatcher,
    "filtering-parallel" -> FilteringParallelMatcher,
    "array-while" -> ArrayWhileMatcher,
    "array-parallel" -> ArrayParallelMatcher
  )

  private val cmdStringToMatcher = cmdStringsAndMatcher.toMap

  def parseFromCmdString(matcherCommandString: String): Option[MatcherFactory] =
    cmdStringToMatcher.get(matcherCommandString)

  val CMD_STRINGS: Seq[String] = cmdStringsAndMatcher.map(_._1)
  val MATCHERS: Seq[MatcherFactory] = cmdStringsAndMatcher.map(_._2)