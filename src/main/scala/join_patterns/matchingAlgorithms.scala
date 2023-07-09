package join_patterns

import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedTransferQueue as Queue
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}
trait Matcher[M, T]:

  type CandidateMatches[M, T] =
    TreeMap[(List[Int], Int), (Map[String, Any], Map[String, Any] => T)]
  object CandidateMatches:
    def apply[M, T](): CandidateMatches[M, T] =
      TreeMap[(List[Int], Int), (Map[String, Any], Map[String, Any] => T)]()(
        Ordering[(List[Int], Int)]
      )

  def apply(q: Queue[M]): T

  def mapIdxsToFits(
      msgIdxsQ: List[Int],
      patternInfo: Set[(Int, M => Boolean, M => Map[String, Any])],
      messages: ListBuffer[M]
  ) =
    msgIdxsQ.foldLeft(Map[Int, Set[(Int, M => Boolean, M => Map[String, Any])]]()) {
      (msgIdxsToFits, msgIdx) =>
        val m = messages(msgIdx)
        val msgIdxToFits = patternInfo.filter { (patIdx, isMsg, fieldExtractor) =>
          isMsg(m)
        }
        msgIdxsToFits.updated(msgIdx, msgIdxToFits)
    }

  def computeValidPermutations(
      msgIdxs: List[Int],
      msgIdxToFits: Map[Int, Set[(Int, M => Boolean, M => Map[String, Any])]]
  ): Iterator[List[(Int, M => Map[String, Any])]] = {
    def isInPattern(msgIdx: Int, msgsInPat: Set[Int]): Boolean =
      msgsInPat.contains(msgIdx)

    def isValidPermutation(permutation: List[Int]): Boolean =
      permutation.forall { msgIdx =>
        val patIdxs     = msgIdxToFits(msgIdx).map(_._1)
        val msgPosInPat = permutation.indexOf(msgIdx)
        isInPattern(msgPosInPat, patIdxs)
      }

    val validPermutations =
      msgIdxs.permutations.collect {
        case permutation if isValidPermutation(permutation) =>
          permutation.map { msgIdx =>
            val possibleFits = msgIdxToFits(msgIdx)
            val msgToPat     = possibleFits.find(pat => pat._1 == permutation.indexOf(msgIdx)).get
            (msgIdx, msgToPat._3)
          }
      }
    validPermutations
  }

  def computeSubsts(messages: ListBuffer[M], possibleFit: List[(Int, M => Map[String, Any])]) =
    possibleFit.foldLeft(Map[String, Any]()) { (substs, idxs) =>
      val (msgIdx, fieldExtractor) = idxs
      val subs                     = fieldExtractor(messages(msgIdx))
      substs ++ subs
    }

  def findBestMatchSubsts(
      validPermutations: Iterator[List[(Int, M => Map[String, Any])]],
      messages: ListBuffer[M],
      pattern: JoinPattern[M, T]
  ) =
    var bestMatchSubsts: Map[String, Any] = null
    val findBestMatch = validPermutations.find { possibleFit =>
      bestMatchSubsts = computeSubsts(messages, possibleFit)
      if pattern.guard(bestMatchSubsts) then true
      else
        bestMatchSubsts = null
        false
    }

    bestMatchSubsts

  // remove all messages from the queue that have been processed
  def removeProcessedMsgs(messages: ListBuffer[M], processedMsgs: List[Int]) =
    messages.zipWithIndex
      .filterNot((_, idx) => processedMsgs.contains(idx))
      .map(_._1)

object SelectMatcher:
  def apply[M, T](algorithm: MatchingAlgorithm, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case MatchingAlgorithm.BasicAlgorithm     => BasicMatcher(patterns)
      case MatchingAlgorithm.TreeBasedAlgorithm => TreeMatcher(patterns)

class BasicMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  def apply(q: Queue[M]): T =
    import scala.jdk.CollectionConverters._

    var result: Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then messages.append(q.take())

      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx

            if messages.size >= pattern.size then
              val candidateMatch = pattern.extract(messages.toList)

              candidateMatch match
                case Some((msgIdxsQ, patternInfo)) =>
                  val msgIdxsToFits = mapIdxsToFits(msgIdxsQ, patternInfo, messages)

                  val validPermutations = computeValidPermutations(msgIdxsQ, msgIdxsToFits)

                  val bestMatchSubsts =
                    findBestMatchSubsts(validPermutations, messages, pattern)

                  if bestMatchSubsts != null then
                    val selectedMatch =
                      (bestMatchSubsts, (substs: Map[String, Any]) => pattern.rhs(substs))

                    candidateMatchesAcc.updated((msgIdxsQ, patternIdx), selectedMatch)
                  else candidateMatchesAcc
                case None => candidateMatchesAcc
            else candidateMatchesAcc
        }

      if candidateMatches.nonEmpty then
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head

        result = Some(rhsFn(substs))

        val unprocessedMsgs = removeProcessedMsgs(messages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs)

      if result.isEmpty then messages.append(q.take())
    result.get

}

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls
  var initMatchingTree = MatchingTree[M](nodeMapping = NodeMapping[M]())
  var patternsWithMatchingTrees = patternsWithIdxs
    .map { patternsWithIdxs =>
      (patternsWithIdxs, initMatchingTree)
    }

  def apply(q: Queue[M]): T =
    import scala.jdk.CollectionConverters._

    var result: Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then messages.append(q.take())

      val (updatedMTs, candidateMatches) =
        patternsWithMatchingTrees.foldLeft(
          (Map[Int, ((JoinPattern[M, T], Int), MatchingTree[M])](), CandidateMatches[M, T]())
        ) { (matchesWithAcc, patternWithMatchingTree) =>
          val (updatedPatternsWithMatchingTrees, candidateMatchesAcc) =
            matchesWithAcc
          val ((pattern, patternIdx), mTree) = patternWithMatchingTree
          val updatedMatchingTree            = pattern.partialExtract(messages.toList, mTree)

          updatedMatchingTree match
            case Some(mTree) =>
              val _updatedMTs = updatedPatternsWithMatchingTrees.updated(
                patternIdx,
                ((pattern, patternIdx), mTree)
              )

              val enoughMsgsToMatch
                  : Option[(List[Int], Set[(Int, M => Boolean, M => Map[String, Any])])] =
                mTree.nodeMapping.view.find((node, fits) =>
                  node.size >= pattern.size && fits.nonEmpty
                )

              enoughMsgsToMatch match
                case Some((msgIdxsQ, patternInfo)) =>
                  val msgIdxsToFits = mapIdxsToFits(msgIdxsQ, patternInfo, messages)

                  val validPermutations = computeValidPermutations(msgIdxsQ, msgIdxsToFits)

                  val bestMatchSubsts =
                    findBestMatchSubsts(validPermutations, messages, pattern)

                  if bestMatchSubsts != null then
                    val selectedMatch =
                      (bestMatchSubsts, (subs: Map[String, Any]) => pattern.rhs(subs))

                    (
                      _updatedMTs,
                      candidateMatchesAcc.updated((msgIdxsQ, patternIdx), selectedMatch)
                    )
                  else
                    val removedNoneValidCandidate = mTree.removeNode(msgIdxsQ)
                    // Prune tree
                    (
                      updatedPatternsWithMatchingTrees.updated(
                        patternIdx,
                        ((pattern, patternIdx), removedNoneValidCandidate)
                      ),
                      candidateMatchesAcc
                    )
                case None => (_updatedMTs, candidateMatchesAcc)

            case None => (updatedPatternsWithMatchingTrees, candidateMatchesAcc)
        }

      patternsWithMatchingTrees = updatedMTs.values.toList

      if candidateMatches.nonEmpty then
        if candidateMatches.nonEmpty then
          val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
          result = Some(rhsFn(substs))

          val unprocessedMsgs = removeProcessedMsgs(messages, candidateQidxs)
          messages.clear()
          messages.addAll(unprocessedMsgs)

          // Prune tree
          patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, mTree) =>
            (joinPat, mTree.pruneTree(candidateQidxs))
          }

      if result.isEmpty then messages.append(q.take())
    result.get
}
