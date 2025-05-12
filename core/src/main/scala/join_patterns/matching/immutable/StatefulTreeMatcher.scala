package join_patterns.matching.immutable

import join_actors.actor.ActorRef
import join_patterns.matching.functions.*
import join_patterns.matching.*
import join_patterns.types.*
import join_patterns.matching.immutable.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.Map as MutMap

class StatefulTreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]])
    extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = MutMap[Int, M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls
  private var patternsWithMatchingTrees: List[PatternState[M, T]] = patternsWithIdxs
    .map { case p @ (pattern, _) =>
      val patInfo                          = pattern.getPatternInfo
      val (initPatternBins, patExtractors) = (patInfo._1, patInfo._2)

      val initMatchingTree = MatchingTree().updated(MessageIdxs(), initPatternBins)
      (p, (initMatchingTree, patInfo))
    }

  /** Finds a match for the given new message added to the matching tree.
    *
    * @param newMsg
    *   The newest message added to the queue.
    * @param patternState
    *   The current pattern state containing the matching tree.0
    * @return
    *   A tuple containing the updated pattern state and the possible candidate match.
    */
  private def findMatch(
      newMsg: (M, Int),
      patternState: PatternState[M, T]
  ): (PatternState[M, T], CandidateMatchOpt[M, T]) =

    val (mQ, mQidx)                               = newMsg
    val ((pattern, patternIdx), (mTree, patInfo)) = patternState
    val updatedMatchingTree                       = internalUpdateMTree(mTree, patInfo, mQ, mQidx)

    updatedMatchingTree match
      case Some(updatedMTree) =>
        val completePatterns = findCompletePatterns(updatedMTree, pattern.size)

        val possibleMatches = completePatterns.iterator
          .map { (msgIdxs, patternBins) =>
            val validPermutations =
              getMsgIdxsWithPayloadExtractor(patInfo.patternExtractors, patternBins)
            val bestMatchOpt = findFairestMatch(validPermutations, messages, pattern)
            bestMatchOpt
          }
          .collectFirst { case Some(_bestMatch) => _bestMatch }

        possibleMatches match
          case Some((bestMatchIdxs, bestMatchSubsts)) =>
            val selectedMatch =
              (
                bestMatchSubsts,
                (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
              )

            val removedNonMatchingNodes =
              updatedMTree.removedAll(completePatterns.removed(bestMatchIdxs).keySet)

            (
              ((pattern, patternIdx), (removedNonMatchingNodes, patInfo)),
              Some((bestMatchIdxs, patternIdx), selectedMatch)
            )
          case None =>
            val removedNonMatchingNodes = updatedMTree.removedAll(completePatterns.keySet)
            (((pattern, patternIdx), (removedNonMatchingNodes, patInfo)), None)

      case None => (patternState, None)

  private def collectCandidateMatches(
      newMsg: (M, Int),
      patternStates: List[PatternState[M, T]]
  ): List[(PatternState[M, T], CandidateMatchOpt[M, T])] =
    patternStates flatMap { patternState =>
      List(findMatch(newMsg, patternState))
    }

  private var mQidx = -1
  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =

    var result: Option[T] = None
    var mQ                = q.take()
    mQidx += 1
    messages.update(mQidx, mQ)

    while result.isEmpty do
      val (updatedPatternStates, possibleMatches) =
        collectCandidateMatches((mQ, mQidx), patternsWithMatchingTrees).unzip

      patternsWithMatchingTrees = updatedPatternStates

      val candidateMatches: CandidateMatches[M, T] =
        possibleMatches.foldLeft(CandidateMatches[M, T]()) {
          case (acc, Some(candidateMatch)) =>
            val (msgIdxs, p) = candidateMatch
            acc.updated(msgIdxs, p)
          case (acc, None) => acc
        }

      if candidateMatches.nonEmpty then
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs, selfRef))

        // Prune tree
        patternsWithMatchingTrees = patternsWithMatchingTrees.map {
          case (joinPat, (currentMTree, pBins)) =>
            val prunedTree = pruneTree(currentMTree, candidateQidxs)
            (joinPat, (prunedTree, pBins))
        }

        // Remove selected message indices from messages
        candidateQidxs.foreach { idx =>
          messages.remove(idx)
        }

      if result.isEmpty then
        mQ = q.take()
        mQidx += 1
        messages.update(mQidx, mQ)

    result.get

  private def internalUpdateMTree(mTree: MatchingTree, patInfo: PatternInfo[M], msg: M, msgIdx: Int): Option[MatchingTree] =
    val extractors = patInfo.patternExtractors

    val matchingMsgIdxsInPattern = extractors.iterator
      .filter { case (_idx, PatternIdxInfo(checkMsgType, _extract, _)) =>
        checkMsgType(msg)
      }
      .map(_._1)
      .to(PatternIdxs)

    matchingMsgIdxsInPattern match
      case PatternIdxs() => Some(mTree)
      case matches =>
        val updatedMTree = updateMTree(mTree, mQidx, matches)
        Some(updatedMTree)

