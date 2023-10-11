package join_patterns

import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

type CandidateMatches[M, T] =
  TreeMap[(List[Int], Int), (Map[String, Any], Map[String, Any] => T)]

object CandidateMatches:
  def apply[M, T](): CandidateMatches[M, T] =
    TreeMap[(List[Int], Int), (Map[String, Any], Map[String, Any] => T)]()(
      Ordering[(List[Int], Int)]
    )
  def printCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]) =
    candidateMatches.foreach { case ((msgIdxs, patIdx), (substs, _)) =>
      println(s"I: ${msgIdxs}, Pattern Index: ${patIdx}, Substs: ${substs}")
    }

trait Matcher[M, T]:
  def apply(q: Mailbox[M]): T

  def mapIdxsToFits(
      msgIdxsQ: List[Int],
      patternInfo: Set[((M => Boolean, M => Map[String, Any]), Int)],
      messages: ListBuffer[M]
  ) =
    msgIdxsQ.foldLeft(Map[Int, Set[((M => Boolean, M => Map[String, Any]), Int)]]()) {
      (msgIdxsToFits, msgIdx) =>
        val m = messages(msgIdx)
        val msgIdxToFits = patternInfo.filter { info =>
          val ((checkMsgType, _), _) = info
          checkMsgType(m)
        }
        msgIdxsToFits.updated(msgIdx, msgIdxToFits)
    }

  def computeValidPermutations(
      msgIdxs: List[Int],
      msgIdxToFits: Map[Int, Set[((M => Boolean, M => Map[String, Any]), Int)]]
  ): Iterator[List[(Int, M => Map[String, Any])]] = {
    def isInPattern(msgIdx: Int, msgsInPat: Set[Int]): Boolean =
      msgsInPat.contains(msgIdx)

    def isValidPermutation(permutation: List[Int]): Boolean =
      permutation.zipWithIndex.forall { (msgIdx, permIdx) =>
        val patIdxs = msgIdxToFits(msgIdx).map(_._2)
        // [3 -> [0, 2], 4 -> [1], 5 -> [0, 2]]

        // P  [0, 1, 2]
        // M  [3, 4, 5]
        // val msgPosInPat = permutation.indexOf(msgIdx)
        isInPattern(permIdx, patIdxs)
      }

    def allPermutations = msgIdxs.permutations
    val validPermutations =
      allPermutations.collect {
        case permutation if isValidPermutation(permutation) =>
          permutation.map { msgIdx =>
            val possibleFits = msgIdxToFits(msgIdx)
            val ((_, extractField), _) =
              possibleFits.find(pat => pat._2 == permutation.indexOf(msgIdx)).get
            (msgIdx, extractField)
          }
      }
    validPermutations
  }

  def computeSubsts(
      messages: ListBuffer[M],
      possibleFit: List[(Int, M => Map[String, Any])]
  ) =
    possibleFit.foldLeft(Map[String, Any]()) { (substsAcc, msgData) =>
      val (msgIdx, extractField) = msgData
      val subs                   = extractField(messages(msgIdx))
      substsAcc ++ subs
    }

  def findBestMatch(
      validPermutations: Iterator[List[(Int, M => Map[String, Any])]],
      messages: ListBuffer[M],
      pattern: JoinPattern[M, T]
  ) =
    var bestMatchSubsts: Map[String, Any] = null
    var bestMatchIdxs: List[Int]          = null
    validPermutations.find { possibleFit =>
      bestMatchSubsts = computeSubsts(messages, possibleFit)
      if pattern.guard(bestMatchSubsts) then
        bestMatchIdxs = possibleFit.map(_._1)
        true
      else false
    }
    if bestMatchIdxs != null && bestMatchSubsts != null then Some((bestMatchIdxs, bestMatchSubsts))
    else None

  // remove all messages from the queue that have been processed
  def removeProcessedMsgs(messages: ListBuffer[(M, Int)], processedMsgs: List[Int]) =
    messages.filterNot((_, idx) => processedMsgs.contains(idx))

object SelectMatcher:
  def apply[M, T](algorithm: MatchingAlgorithm, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case MatchingAlgorithm.BasicAlgorithm     => BasicMatcher(patterns)
      case MatchingAlgorithm.TreeBasedAlgorithm => TreeMatcher(patterns)

class BasicMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  def apply(q: Mailbox[M]): T =
    import scala.jdk.CollectionConverters._

    var result: Option[T] = None

    if messages.isEmpty then messages.append(q.take())
    while result.isEmpty do
      val indexedMessages = messages.zipWithIndex
      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            if messages.size >= pattern.size then
              val possibleMatches = pattern.extract(messages.toList)
              possibleMatches match
                case Some((candidateIdxsQ, patternInfo)) =>
                  val bestMatch = candidateIdxsQ
                    .map { candidateI =>
                      val msgIdxsToFits =
                        mapIdxsToFits(candidateI, patternInfo, messages)

                      val validPermutations = computeValidPermutations(candidateI, msgIdxsToFits)
                      findBestMatch(validPermutations, messages, pattern)
                    }
                    .collectFirst({ case Some(_bestMatch) => _bestMatch })

                  bestMatch match {
                    case Some((bestMatchIdxs, bestMatchSubsts)) =>
                      // println(s"bestMatchIdxs: $bestMatchIdxs -- bestMatchSubsts: $bestMatchSubsts")
                      val selectedMatch =
                        (bestMatchSubsts, (substs: Map[String, Any]) => pattern.rhs(substs))
                      // println(s"Selected match: $selectedMatch")
                      candidateMatchesAcc.updated((bestMatchIdxs, patternIdx), selectedMatch)
                    case None => candidateMatchesAcc
                  }

                case None => candidateMatchesAcc
            else candidateMatchesAcc
        }
      if candidateMatches.nonEmpty then
        // CandidateMatches.printCandidateMatches(candidateMatches)
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head

        result = Some(rhsFn(substs))

        val unprocessedMsgs = removeProcessedMsgs(indexedMessages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs.map(_._1))

      if result.isEmpty then messages.append(q.take())

    result.get

}

class TreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Messages extracted from the queue are saved here to survive across apply() calls
  val messages                 = ListBuffer[(M, Int)]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls
  var initMatchingTree = MatchingTree[M](nodeMapping = NodeMapping[M]())
  var patternsWithMatchingTrees = patternsWithIdxs
    .map { patternsWithIdxs =>
      (patternsWithIdxs, initMatchingTree)
    }

  var mQidx = -1
  def apply(q: Mailbox[M]): T =
    import scala.jdk.CollectionConverters._

    var result: Option[T] = None
    var mQ                = q.take()
    mQidx += 1
    messages.append((mQ, mQidx))
    while result.isEmpty do
      val (updatedMTs, candidateMatches) =
        patternsWithMatchingTrees.foldLeft(
          (Map[Int, ((JoinPattern[M, T], Int), MatchingTree[M])](), CandidateMatches[M, T]())
        ) { (matchesWithAcc, patternWithMatchingTree) =>
          val (updatedPatternsWithMatchingTrees, candidateMatchesAcc) = matchesWithAcc
          val ((pattern, patternIdx), mTree)                          = patternWithMatchingTree
          val updatedMatchingTree = pattern.partialExtract((mQ, mQidx), mTree)

          updatedMatchingTree match
            case Some(currentMTree) =>
              val _updatedMTs = updatedPatternsWithMatchingTrees.updated(
                patternIdx,
                ((pattern, patternIdx), currentMTree)
              )
              val enoughMsgsToMatch
                  : Option[(List[Int], Set[((M => Boolean, M => Map[String, Any]), Int)])] =
                currentMTree.nodeMapping.view.find((node, fits) =>
                  node.size == pattern.size && fits.nonEmpty && fits.size == pattern.size
                )

              enoughMsgsToMatch match
                case Some((msgIdxsQ, patternInfo)) =>
                  val msgIdxsToFits = mapIdxsToFits(msgIdxsQ, patternInfo, messages.map(_._1))

                  val validPermutations = computeValidPermutations(msgIdxsQ, msgIdxsToFits)

                  val bestMatch =
                    findBestMatch(validPermutations, messages.map(_._1), pattern)

                  bestMatch match {
                    case Some((bestMatchIdxs, bestMatchSubsts)) =>
                      // println(s"bestMatchIdxs: $bestMatchIdxs -- bestMatchSubsts: $bestMatchSubsts")
                      val selectedMatch =
                        (bestMatchSubsts, (substs: Map[String, Any]) => pattern.rhs(substs))
                      (
                        _updatedMTs,
                        candidateMatchesAcc.updated((bestMatchIdxs, patternIdx), selectedMatch)
                      )

                    case None =>
                      // println(s"Removing node: $msgIdxsQ")
                      val removedNoneValidCandidate = currentMTree.removeNode(msgIdxsQ)
                      (
                        updatedPatternsWithMatchingTrees.updated(
                          patternIdx,
                          ((pattern, patternIdx), removedNoneValidCandidate)
                        ),
                        candidateMatchesAcc
                      )
                  }
                case None => (_updatedMTs, candidateMatchesAcc)

            case None => (updatedPatternsWithMatchingTrees, candidateMatchesAcc)
        }

      patternsWithMatchingTrees = updatedMTs.values.toList

      if candidateMatches.nonEmpty then
        // CandidateMatches.printCandidateMatches(candidateMatches)
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs))

        // Prune tree
        patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, currentMTree) =>
          (joinPat, currentMTree.pruneTree(candidateQidxs))
        }

      if result.isEmpty then
        mQ = q.take()
        mQidx += 1
        messages.append((mQ, mQidx))
    result.get
}
