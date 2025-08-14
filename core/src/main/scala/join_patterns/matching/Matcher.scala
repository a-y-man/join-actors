package join_patterns.matching

import join_actors.actor.ActorRef
import join_patterns.types.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.Console
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.TreeMap

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
    TreeMap[MatchIdxs, (LookupEnv, RHSFnClosure[M, T])]()(using
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
  * @tparam M the message type that the matcher will process
  * @tparam T the result type produced by successful pattern matching
  */
trait Matcher[M, T]:

  /** The matcher constructor that takes a mailbox and an actor reference and returns the result of
    * the join pattern.
    *
    * @param q
    *   The mailbox where messages sent to the actor are queued. This is an alias for the
    *   `LinkedTransferQueue[M]` data structure.
    * @param selfRef
    *   The actor reference where the matcher is used
    * @return
    *   The result of the join pattern.
    */
  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T

/**
 * Factory trait for creating Matcher instances.
 * 
 * A MatcherFactory provides a way to create matchers that can process join definitions
 * and determine how to match incoming messages against join patterns.
 * 
 * @tparam M the message type that the matcher will process
 * @tparam T the result type produced by successful pattern matching
 */
trait MatcherFactory:
  def apply[M, T]: JoinDefinition[M, T] => Matcher[M, T]
