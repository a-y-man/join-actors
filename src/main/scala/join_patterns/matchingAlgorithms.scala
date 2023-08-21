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

  def printCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]) =
    candidateMatches.foreach { case ((msgIdxs, patIdx), (substs, _)) =>
      println(s"I: ${msgIdxs}, Pattern Index: ${patIdx}, Substs: ${substs}")
    }

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

    val validPermutations =
      msgIdxs.permutations.collect {
        case permutation if isValidPermutation(permutation) =>
          permutation.map { msgIdx =>
            val possibleFits = msgIdxToFits(msgIdx)
            val msgToPat     = possibleFits.find(pat => pat._2 == permutation.indexOf(msgIdx)).get
            (msgIdx, msgToPat._1._2)
          }
      }
    validPermutations
  }

  def computeSubsts(messages: ListBuffer[M], possibleFit: List[(Int, M => Map[String, Any])]) =
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
      // println(s"Queue: ${messages.mkString(", ")}")

      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            if messages.size >= pattern.size then
              // println(s"candidateMatchesAcc: $candidateMatchesAcc")
              val possibleMatches = pattern.extract(messages.toList)
              possibleMatches match
                case Some((candidateIdxsQ, patternInfo)) =>
                  // val cop = candidateIdxsQ.toList
                  // println(s"candidateIdxsQ: $cop")
                  val bestMatch = candidateIdxsQ
                    .map { candidateI =>
                      val msgIdxsToFits = mapIdxsToFits(candidateI, patternInfo, messages)

                      val validPermutations = computeValidPermutations(candidateI, msgIdxsToFits)

                      // println(s"candidateI: $candidateI")
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
      // printCandidateMatches(candidateMatches)
      if candidateMatches.nonEmpty then
        // printCandidateMatches(candidateMatches)
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head

        result = Some(rhsFn(substs))

        val unprocessedMsgs = removeProcessedMsgs(messages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs)

      if result.isEmpty then
        messages.append(q.take())
        // println(s"Queue: ${messages.mkString(", ")}")

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
    var mQ: Option[M]     = None
    var mQidx             = -1

    while result.isEmpty do
      // println(s"mQidx: $mQidx")
      // println(s"mQ: $mQ")
      if messages.isEmpty then
        mQ = Some(q.take())
        messages.append(mQ.get)
        mQidx += 1

      val (updatedMTs, candidateMatches) =
        patternsWithMatchingTrees.foldLeft(
          (Map[Int, ((JoinPattern[M, T], Int), MatchingTree[M])](), CandidateMatches[M, T]())
        ) { (matchesWithAcc, patternWithMatchingTree) =>
          val (updatedPatternsWithMatchingTrees, candidateMatchesAcc) =
            matchesWithAcc
          val ((pattern, patternIdx), mTree) = patternWithMatchingTree
          val updatedMatchingTree            = pattern.partialExtract((mQ.get, mQidx), mTree)
          // println("===========================================================")
          // println(s"patternIdx: $patternIdx --- mQidx: $mQidx -- mQ: $mQ")
          // printMapping(updatedMatchingTree.get.nodeMapping)
          // println("-----------------------------------------------------------")

          updatedMatchingTree match
            case Some(mTree) =>
              val _updatedMTs = updatedPatternsWithMatchingTrees.updated(
                patternIdx,
                ((pattern, patternIdx), mTree)
              )
              val enoughMsgsToMatch
                  : Option[(List[Int], Set[((M => Boolean, M => Map[String, Any]), Int)])] =
                mTree.nodeMapping.view.find((node, fits) =>
                  node.size == pattern.size && fits.nonEmpty && fits.size == pattern.size
                )
              // println(s"enoughMsgsToMatch: $enoughMsgsToMatch")
              enoughMsgsToMatch match
                case Some((msgIdxsQ, patternInfo)) =>
                  val msgIdxsToFits = mapIdxsToFits(msgIdxsQ, patternInfo, messages)

                  val validPermutations = computeValidPermutations(msgIdxsQ, msgIdxsToFits)

                  val bestMatch =
                    findBestMatch(validPermutations, messages, pattern)

                  bestMatch match {
                    case Some((bestMatchIdxs, bestMatchSubsts)) =>
                      val selectedMatch =
                        (bestMatchSubsts, (substs: Map[String, Any]) => pattern.rhs(substs))
                      // println(s"bestMatchIdxs: $bestMatchIdxs")

                      (
                        _updatedMTs,
                        candidateMatchesAcc.updated((bestMatchIdxs, patternIdx), selectedMatch)
                      )

                    case None =>
                      val removedNoneValidCandidate = mTree.removeNode(msgIdxsQ)
                      // println(s"removedNoneValidCandidate: $msgIdxsQ")
                      // printMapping(removedNoneValidCandidate.nodeMapping)
                      // Prune tree
                      (
                        updatedPatternsWithMatchingTrees.updated(
                          patternIdx,
                          ((pattern, patternIdx), removedNoneValidCandidate)
                        ),
                        candidateMatchesAcc
                      )
                    // (_updatedMTs, candidateMatchesAcc)
                  }
                case None => (_updatedMTs, candidateMatchesAcc)

            case None => (updatedPatternsWithMatchingTrees, candidateMatchesAcc)
        }

      patternsWithMatchingTrees = updatedMTs.values.toList

      // patternsWithMatchingTrees.foreach { case ((pattern, patternIdx), mTree) =>
      //   println(s"patternIdx: $patternIdx")
      //   printMapping(mTree.nodeMapping)
      // }

      if candidateMatches.nonEmpty then
        // printCandidateMatches(candidateMatches)
        // println(s"!!MATCH!! candidateMatches: $candidateMatches")
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs))

        val unprocessedMsgs = removeProcessedMsgs(messages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs)
        mQidx = -1

        // Prune tree
        patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, mTree) =>
          (joinPat, mTree.pruneTree(candidateQidxs))
        }

        // patternsWithMatchingTrees.foreach { case ((pattern, patternIdx), mTree) =>
        //   println(s"patternIdx: $patternIdx")
        //   printMapping(mTree.nodeMapping)
        // }

      if result.isEmpty then
        // println(s"mQidx': $mQidx")
        // println(s"mQ': $mQ")
        mQ = Some(q.take())
        messages.append(mQ.get)
        mQidx += 1
    result.get
}
