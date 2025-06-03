package join_patterns.matching.functions

import join_patterns.matching.*
import join_patterns.matching.immutable.MatchingTree
import join_patterns.types.{*, given}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Map as MutMap

private def crossProduct[A](
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

private def computeValidCombinations(
                              patternBins: PatternBins
                            ): LazyList[LazyList[(PatternIdx, Int)]] =
  val combs = patternBins.view
    .flatMap((patternShape, messageIdxs) =>
      val msgsPermutation = messageIdxs
        .combinations(patternShape.size)
        .map(_.permutations)
        .map(l => l.map(patternShape zip _).to(LazyList))
      TreeMap(patternShape -> msgsPermutation.to(LazyList))
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
    val PatternIdxInfo(_, extractField, _) = patExtractors(pidx)
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
private def computeSubsts[M](
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
def findFairestMatch[M, T](
                            validPermutations: Iterator[List[(Int, M => LookupEnv)]],
                            messages: MutMap[Int, M],
                            pattern: JoinPattern[M, T]
                          ): Option[(MessageIdxs, LookupEnv)] =
  var bestMatchSubsts: LookupEnv = null
  var bestMatchIdxs: MessageIdxs = null
  validPermutations.find { possibleFit =>
    bestMatchSubsts = computeSubsts(messages, possibleFit)
    if pattern.guard(bestMatchSubsts) then
      bestMatchIdxs = MessageIdxs(possibleFit.map(_._1)*)
      true
    else false
  }
  if bestMatchIdxs != null && bestMatchSubsts != null then Some((bestMatchIdxs.sorted, bestMatchSubsts))
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
def removeProcessedMsgs[M](messages: MutMap[Int, M], processedMsgs: MessageIdxs): MutMap[Int, M] =
  // messages.filterNot((_, idx) => processedMsgs.contains(idx))
  messages --= processedMsgs

def appendToFile(filename: String, content: String): Unit =
  val fileFolder = os.pwd / "core" / "logs"
  os.makeDir.all(fileFolder)
  os.write.append(fileFolder / filename, content)

def logMTreeAndMailBoxSize[M](mTrees: List[MatchingTree], msgCount: Int): String =
  val mTreesSize = mTrees.map(_.size - 1).sum
  s"${msgCount + 1},$mTreesSize"

def logMailBoxSizeAndMsgCnt[M](mBox: MutMap[Int, M], msgCount: Int): String =
  s"$msgCount,${mBox.size}"