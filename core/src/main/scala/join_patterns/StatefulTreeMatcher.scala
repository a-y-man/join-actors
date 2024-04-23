package join_patterns

import actor.ActorRef

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.ArrayBuffer

class StatefulTreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]])
    extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ArrayBuffer[(M, Int)]()
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
  ): (PatternState[M, T], CandidateMatch[M, T]) =

    val (mQ, mQidx)                               = newMsg
    val ((pattern, patternIdx), (mTree, patInfo)) = patternState
    val updatedMatchingTree                       = pattern.updateMTree((mQ, mQidx), mTree)

    updatedMatchingTree match
      case Some(updatedMTree) =>
        val completePatterns = findCompletePatterns(updatedMTree, pattern.size)

        val possibleMatches = completePatterns.iterator
          .map { (msgIdxs, patternBins) =>
            val validPermutations =
              getMsgIdxsWithPayloadExtractor(patInfo.patternExtractors, patternBins)
            val bestMatchOpt = findFairestMatch(validPermutations, messages.map(_._1), pattern)
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
  ): List[(PatternState[M, T], CandidateMatch[M, T])] =
    patternStates flatMap { patternState =>
      List(findMatch(newMsg, patternState))
    }

  private var mQidx = -1
  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    import scala.jdk.CollectionConverters.*

    var result: Option[T] = None
    var mQ                = q.take()
    mQidx += 1
    messages.append((mQ, mQidx))

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

      if result.isEmpty then
        mQ = q.take()
        mQidx += 1
        messages.append((mQ, mQidx))

    result.get
