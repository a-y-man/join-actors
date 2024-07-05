package join_patterns

import actor.ActorRef
import os.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.ArrayBuffer

def logMTreeAndMailBoxSize[M](mTrees: List[MatchingTree], msgCount: Int): String =
  val mTreesSize = mTrees.map(_.size - 1).reduce(_ + _)
  s"${msgCount + 1},$mTreesSize"

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

  // val filename0 = "stateful_tree_matcher_mtree_size_0_random_msgs_3_valid_msgs_V4.csv"
  // val filename0 = "stateful_tree_matcher_mtree_size_3_random_msgs_3_valid_msgs_V4.csv"
  // val filename0 = "stateful_tree_matcher_mtree_size_6_random_msgs_3_valid_msgs_V4.csv"

  // val logs: ArrayBuffer[String] = ArrayBuffer()
  // logs.append("Message Count,Matching Trees Size")
  // appendToFile(filename0, logs.head + "\n" + "0,0\n")

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
      // val mTrees     = patternsWithMatchingTrees.map(_._2._1)
      // val currentLog = logMTreeAndMailBoxSize(mTrees, mQidx)
      // logs.append(currentLog)
      // // println(logs.mkString("\n"))
      // appendToFile(filename0, currentLog + "\n")

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
