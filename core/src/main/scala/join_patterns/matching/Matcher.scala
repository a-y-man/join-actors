package join_patterns.matching

import join_actors.actor.ActorRef
import join_patterns.matching.array_while.ArrayWhileMatcher
import join_patterns.matching.buffer_while.BufferWhileMatcher
import join_patterns.matching.brute_force.BruteForceMatcher
import join_patterns.matching.eager_parallel.EagerParallelMatcher
import join_patterns.matching.filtering_parallel.FilteringParallelMatcher
import join_patterns.matching.filtering_while.FilteringWhileMatcher
import join_patterns.matching.while_eager.WhileEagerMatcher
import join_patterns.matching.immutable.StatefulTreeMatcher
import join_patterns.matching.lazy_mutable.LazyMutableMatcher
import join_patterns.matching.lazy_parallel.LazyParallelMatcher
import join_patterns.matching.mutable.MutableStatefulMatcher
import join_patterns.matching.while_lazy.WhileLazyMatcher
import join_patterns.types.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.Console
import scala.collection.immutable.{ArraySeq, TreeMap}

type RHSFnClosure[M, T] = (LookupEnv, ActorRef[M]) => T

/** Type alias representing the sub-sequence of message indices and the matched pattern indices.
  *
  * @param messageIdxs
  *   The indices of the matched messages.
  * @param patternIdx
  *   The index of the join pattern in the join defintion.
  */
type MatchIdxs = (MessageIdxs, PatternIdx)

/** A candidate match of a join pattern where the key is a sub-sequence of message indices that are
  * the fairest match for a join pattern and the value is a tuple of the substitutions and the RHS
  * function that takes the substitutions and the actor reference as arguments and returns the
  * result of the join pattern RHS.
  *
  * @tparam M
  *   The type of messages in the queue.
  * @tparam T
  *   The type of the RHS of the join pattern.
  */
type CandidateMatch[M, T] = (MatchIdxs, (LookupEnv, RHSFnClosure[M, T]))
type CandidateMatchOpt[M, T] = Option[CandidateMatch[M, T]]

/** A map of candidate matches in the join definition where the key is a sub-sequence of message
  * indices that are the fairest match for a join pattern and the value is a tuple of the
  * substitutions and the RHS function closure.
  *
  * @tparam M
  *   The type of messages in the queue.
  * @tparam T
  *   The type of the RHS of the join pattern.
  */
type CandidateMatches[M, T] =
  TreeMap[MatchIdxs, (LookupEnv, RHSFnClosure[M, T])]

object CandidateMatches:
  import math.Ordering.Implicits.seqOrdering
  private val defaultSeqOrderingForMessageIdxs = seqOrdering[ArraySeq, Int]

  def apply[M, T](): CandidateMatches[M, T] =
    TreeMap[MatchIdxs, (LookupEnv, RHSFnClosure[M, T])]()(
      Ordering.Tuple2[MessageIdxs, PatternIdx](using defaultSeqOrderingForMessageIdxs)
    )

  def logCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]): Unit =
    val stringifiedMatches =
      candidateMatches
        .map { case ((msgIdxs, patIdx), (substs, _)) =>
          val ppMsgIdxs =
            s"${Console.UNDERLINED}${Console.GREEN}I: ${msgIdxs.mkString("[", ", ", "]")}${Console.RESET}"
          val ppPatIdx =
            s"${Console.UNDERLINED}${Console.RED}Pattern Idx: ${patIdx}${Console.RESET}"
          val ppSubsts =
            s"${Console.UNDERLINED}${Console.BLUE}Substs:${Console.RESET} ${ppLookupEnv(substs)}${Console.RESET}"
          s"${ppMsgIdxs} -- ${ppPatIdx} -- ${ppSubsts}"
        }
        .mkString("\n")

    println {
      s"\n===================CandidateMatches===================\n" +
        s"${stringifiedMatches}" +
        s"\n======================================================\n"
    }

/** A matcher trait the defines the interface for a join pattern matcher.
  *
  * @tparam M
  *   The type of messages in the queue.
  * @tparam T
  *   The type of the RHS of the join pattern.
  */
trait Matcher[M, T]:

  /** The matcher constructor that takes a mailbox and an actor reference and returns the result of
    * the join pattern.
    *
    * @param q
    *   The mailbox containing the messages.
    * @param selfRef
    *   The actor reference where the matcher is used
    * @return
    *   The result of the join pattern.
    */
  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T

object SelectMatcher:
  import MatchingAlgorithm.*
  def apply[M, T](algorithm: MatchingAlgorithm, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case BruteForceAlgorithm        => BruteForceMatcher(patterns)
      case StatefulTreeBasedAlgorithm => StatefulTreeMatcher(patterns)
      case MutableStatefulAlgorithm   => MutableStatefulMatcher(patterns)
      case LazyMutableAlgorithm       => LazyMutableMatcher(patterns)
      case WhileLazyAlgorithm         => WhileLazyMatcher(patterns)
      case FilteringWhileAlgorithm    => FilteringWhileMatcher(patterns)
      case WhileEagerAlgorithm        => WhileEagerMatcher(patterns)
      case EagerParallelAlgorithm(numThreads)     => EagerParallelMatcher(patterns, numThreads)
      case LazyParallelAlgorithm(numThreads)      => LazyParallelMatcher(patterns, numThreads)
      case FilteringParallelAlgorithm(numThreads) => FilteringParallelMatcher(patterns, numThreads)
      case ArrayWhileAlgorithm => ArrayWhileMatcher(patterns)
      case BufferWhileAlgorithm => BufferWhileMatcher(patterns)
