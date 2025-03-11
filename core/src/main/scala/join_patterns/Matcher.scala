package join_patterns.matcher

import join_actors.actor.ActorRef
import join_patterns.lazy_mutable.LazyMutableTreeMatcher
import join_patterns.types.*
import join_patterns.types.given
import join_patterns.utils.*
import join_patterns.matching_tree.*
import join_patterns.matching_tree.given
import join_patterns.matcher.brute_force_matcher.*
import join_patterns.matcher.stateful_tree_matcher.*
import join_patterns.mutable.MutableStatefulTreeMatcher

import java.util.concurrent.LinkedTransferQueue as Mailbox
import java.util.concurrent.TimeUnit
import scala.Console
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map as MutMap

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
type CandidateMatch[M, T] = Option[(MatchIdxs, (LookupEnv, RHSFnClosure[M, T]))]

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
  import math.Ordering.Implicits.infixOrderingOps
  import math.Ordering.Implicits.seqOrdering
  def apply[M, T](): CandidateMatches[M, T] =
    TreeMap[MatchIdxs, (LookupEnv, RHSFnClosure[M, T])]()(
      Ordering.Tuple2[MessageIdxs, PatternIdx]
    )

  def logCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]) =
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

  def crossProduct[A](
      listOfLists: LazyList[LazyList[A]]
  ): LazyList[LazyList[A]] =
    /** The following `crossProduct` method is adapted from the following Stack Overflow answer:
      * https://stackoverflow.com/a/54333046/
      */
    listOfLists match
      case LazyList()       => LazyList()
      case x #:: LazyList() => x.map(LazyList(_))
      case x #:: xs =>
        val tail = crossProduct(xs)
        for
          i <- x
          j <- tail
        yield LazyList(i) #::: j

  def computeValidCombinations(
      patternBins: PatternBins
  ) =
    val combs = patternBins.view
      .flatMap((patternShape, messageIdxs) =>
        val msgsPermutation = messageIdxs
          .combinations(patternShape.size)
          .map(_.permutations)
          .map(l => l.map(patternShape zip _).to(LazyList))
        TreeMap(patternShape -> msgsPermutation.to(LazyList))(using messageIdxOrdering)
      )
    crossProduct(
      combs.map(_._2).to(LazyList)
    ).flatMap(crossProduct(_)).map(_.flatten)

  def getMsgIdxsWithPayloadExtractor[M](
      patExtractors: PatternExtractors[M],
      patternBins: PatternBins
  ): Iterator[List[(Int, M => Map[String, Any])]] =
    val validCombinations = computeValidCombinations(patternBins).iterator
    for
      combination <- validCombinations
      validCombination = combination.sortBy(_._1)
    yield validCombination.map { case (pidx, msgIdx) =>
      val (_, extractField) = patExtractors(pidx)
      (msgIdx, extractField)
    }.toList

  /** Computes the substitutions for a list of messages based on a list of possible fits.
    *
    * @param messages
    *   The matching sub-sequence of messages to compute substitutions for.
    * @param possibleFit
    *   The list of possible fits, where each fit is represented as a tuple containing the index in
    *   the join pattern where the message fits and a function that extracts a LookupEnv from a
    *   message.
    * @return
    *   The computed substitutions as a LookupEnv.
    */
  def computeSubsts(
      messages: MutMap[Int, M],
      possibleFit: List[(Int, M => LookupEnv)]
  ): LookupEnv =
    possibleFit.foldLeft(LookupEnv.empty) { (substsAcc, msgData) =>
      val (msgIdx, extractField) = msgData
      val subs                   = extractField(messages(msgIdx))
      substsAcc ++ subs
    }

  /** Finds the fairest match for a given join pattern. This function filters out message
    * combinations that do not satisfy the guard of the join pattern.
    *
    * @param validPermutations
    *   An iterator of valid permutations of join pattern and a function that computes
    *   substitutions.
    * @param messages
    *   A potential sub-sequence of messages to be matched.
    * @param pattern
    *   The join pattern to be matched against.
    * @return
    *   The fairest match for the given join pattern.
    */
  def findFairestMatch(
      validPermutations: Iterator[List[(Int, M => LookupEnv)]],
      messages: MutMap[Int, M],
      pattern: JoinPattern[M, T]
  ) =
    var bestMatchSubsts: LookupEnv = null
    var bestMatchIdxs: MessageIdxs = null
    validPermutations.find { possibleFit =>
      bestMatchSubsts = computeSubsts(messages, possibleFit)
      if pattern.guard(bestMatchSubsts) then
        bestMatchIdxs = MessageIdxs(possibleFit.map(_._1)*)
        true
      else false
    }
    if bestMatchIdxs != null && bestMatchSubsts != null then Some((bestMatchIdxs, bestMatchSubsts))
    else None

  /** Removes processed messages from the mailbox.
    *
    * @param messages
    *   The list of messages to filter.
    * @param processedMsgs
    *   The set of indices of processed messages.
    * @return
    *   The filtered list of messages without the processed messages.
    */
  def removeProcessedMsgs(messages: MutMap[Int, M], processedMsgs: MessageIdxs) =
    // messages.filterNot((_, idx) => processedMsgs.contains(idx))
    messages --= processedMsgs

  def appendToFile(filename: String, content: String): Unit =
    val fileFolder = os.pwd / "core" / "logs"
    os.makeDir.all(fileFolder)
    os.write.append(fileFolder / filename, content)

object SelectMatcher:
  import MatchingAlgorithm.*
  def apply[M, T](algorithm: MatchingAlgorithm, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case BruteForceAlgorithm        => BruteForceMatcher(patterns)
      case StatefulTreeBasedAlgorithm => StatefulTreeMatcher(patterns)
      case MutableStatefulAlgorithm   => MutableStatefulTreeMatcher(patterns)
      case LazyMutableAlgorithm       => LazyMutableTreeMatcher(patterns)
