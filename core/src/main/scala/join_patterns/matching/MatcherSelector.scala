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

val numCores = Runtime.getRuntime().availableProcessors()

object MatcherSelector:
  private val cmdStringsAndMatcher = List(
    "brute" -> BruteForceMatcher,
    "stateful" -> StatefulTreeMatcher,
    "mutable" -> MutableStatefulMatcher,
    "lazy-mutable" -> LazyMutableMatcher,
    "while-lazy" -> WhileLazyMatcher,
    "filtering-while" -> FilteringWhileMatcher,
    "while-eager" -> WhileEagerMatcher,
    "eager-parallel" -> EagerParallelMatcher(numCores),
    "lazy-parallel" -> LazyParallelMatcher(numCores),
    "filtering-parallel" -> FilteringParallelMatcher(numCores),
    "array-while" -> ArrayWhileMatcher,
    "array-parallel" -> ArrayParallelMatcher(numCores)
  )

  private val cmdStringToMatcher = cmdStringsAndMatcher.toMap

  def parseFromCmdString(matcherCommandString: String): Option[MatcherFactory] =
    cmdStringToMatcher.get(matcherCommandString)

  val CMD_STRINGS: Seq[String] = cmdStringsAndMatcher.map(_._1)
  val MATCHERS: Seq[MatcherFactory] = cmdStringsAndMatcher.map(_._2)
