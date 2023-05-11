package join_patterns

import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedTransferQueue as Queue
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}
trait Matcher[M, T]:
  def apply(q: Queue[M]): T

object SelectMatcher:
  def apply[M, T](algorithm: MatchingAlgorithm, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case MatchingAlgorithm.BasicAlgorithm     => BasicMatcher(patterns)
      case MatchingAlgorithm.TreeBasedAlgorithm => TreeMatcher(patterns)

def compareIndices(i1WithPatIdx: (Int, List[Int]), i2WithPatIdx: (Int, List[Int])): Boolean =
  import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

  val (_, i1) = i1WithPatIdx
  val (_, i2) = i2WithPatIdx

  val i1sorted = i1.sorted
  val i2sorted = i2.sorted

  (i1sorted < i2sorted) || ((i1sorted == i2sorted) && (i1 < i2))

class BasicMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Type aliases used only within this class
  type CandidateMatches[M, T] = Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]
  object CandidateMatches:
    def apply[M, T](): CandidateMatches[M, T] =
      Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]()

  type Index[M] = MutMap[(Int, List[Int]), (List[M], Map[String, Any])]
  object Index:
    def apply[M](): Index[M] =
      MutMap[(Int, List[Int]), (List[M], Map[String, Any])]()

  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then messages.append(q.take())
      val index = Index[M]()
      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            if messages.size >= pattern.size then
              val (candidateMatch, idxMsgs, substs) = pattern.extract(messages.toList)
              candidateMatch match
                case Some(candMatch) =>
                  val (msgIdxsQ, msgPattern) = idxMsgs.unzip

                  if pattern.guard(substs) then
                    index((patternIdx, msgIdxsQ)) = (msgPattern, substs)
                    candidateMatchesAcc.updated(
                      (patternIdx, candMatch, substs),
                      (subs: Map[String, Any]) => pattern.rhs(subs)
                    )
                  else candidateMatchesAcc
                case None => candidateMatchesAcc
            else candidateMatchesAcc
        }

      val candidateIndexes = index.keys.toList.sortWith(compareIndices)
      // println(s"Candidate idxs\n${candidateIndexes.mkString("\n")}")
      if !candidateIndexes.isEmpty then
        val (patternIdx, msgIdxQ) = candidateIndexes.head
        val (msgs, subs)          = index(candidateIndexes.head)
        val selectedMatch         = (patternIdx, msgs, subs)

        result = for {
          rhsFn <- candidateMatches.get(selectedMatch)
        } yield rhsFn(subs)

        messages.subtractAll(msgs)

      if result.isEmpty then messages.append(q.take())
    result.get

}

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Type alias used only within this class
  type CandidateMatches[M, T] =
    TreeMap[(Int, List[Int]), (Map[String, Any], Map[String, Any] => T)]
  object CandidateMatches:
    def apply[M, T](): CandidateMatches[M, T] =
      TreeMap[(Int, List[Int]), (Map[String, Any], Map[String, Any] => T)]()

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
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then messages.append(q.take())

      val (updatedMTs, candidateMatches) =
        patternsWithMatchingTrees.foldLeft(
          (Map[Int, ((JoinPattern[M, T], Int), MatchingTree[M])](), CandidateMatches[M, T]())
        ) { (matchesWithAcc, patternWithMatchingTree) =>
          val (updatedPatternsWithMatchingTrees, matches) =
            matchesWithAcc
          val ((pattern, patternIdx), mTree) = patternWithMatchingTree
          val updatedMatchingTree            = pattern.partialExtract(messages.toList, mTree)

          updatedMatchingTree match
            case Some(mTree) =>
              val _updatedMTs = updatedPatternsWithMatchingTrees.updated(
                patternIdx,
                ((pattern, patternIdx), mTree)
              )

              val enoughMsgsToMatch =
                mTree.nodeMapping.view.find((node, fits) =>
                  node.size >= pattern.size && fits.nonEmpty
                )

              if enoughMsgsToMatch.nonEmpty then
                val possibleFits = enoughMsgsToMatch.toMap.keySet.flatMap { msgIdxs =>
                  msgIdxs.flatMap { i =>
                    val res = enoughMsgsToMatch
                      .to(TreeMap)(msgIdxs)
                      .flatMap { (patIdx, isMsg, fieldExtractor) =>
                        val m = messages(i)
                        if isMsg(m) then Some(patIdx -> fieldExtractor(m))
                        else None
                      }
                    if res.nonEmpty then Some((i, res.toMap))
                    else None
                  }
                }.toList

                val matchesWithSubsts = possibleFits.permutations
                  .map { permutation =>
                    permutation.foldLeft((List[Int](), Set[Int](), Map[String, Any]())) {
                      (acc, mapping) =>
                        val (msgIdx, possibleFitToSubsts) = mapping
                        val (seenMsgs, seenPats, substs)  = acc
                        val patIdx =
                          possibleFitToSubsts.keySet.find(k => !seenPats.contains(k)).get
                        (
                          seenMsgs :+ msgIdx,
                          seenPats + patIdx,
                          substs ++ possibleFitToSubsts(patIdx)
                        )
                    }
                  }
                  .map((msgIdxs, _, substs) => (msgIdxs, substs))

                val whereGuardTrue = matchesWithSubsts
                  .filter((idxs, substs) => pattern.guard(substs))

                val selectedMatch = whereGuardTrue.map { (msgIdxs, substs) =>
                  (
                    (patternIdx, msgIdxs),
                    (substs, (subs: Map[String, Any]) => pattern.rhs(subs))
                  )
                }.toList

                (
                  _updatedMTs,
                  matches ++ selectedMatch
                )
              else (_updatedMTs, matches)

            case None => (updatedPatternsWithMatchingTrees, matches)
        }

      patternsWithMatchingTrees = updatedMTs.values.toList

      // patternsWithMatchingTrees.foreach { (jp, mTree) =>
      //   println(s"MT for JP ${jp._2}")
      //   printMapping(mTree.nodeMapping)
      // }

      if candidateMatches.nonEmpty then
        if candidateMatches.nonEmpty then
          val candidateQidxs = candidateMatches.keys.toList.sortWith(compareIndices)
          // println(s"Candidate idxs\n${candidateQidxs.mkString("\n")}")

          val candidateRHS = candidateMatches.get(candidateQidxs.head)
          if candidateRHS.nonEmpty then
            val (subst, rhsFn) = candidateRHS.head
            result = Some(rhsFn(subst))

            val unprocessedMsgs = messages.zipWithIndex
              .filterNot((_, idx) => candidateQidxs.head._2.contains(idx))
              .map(_._1)
            messages.clear()
            messages.addAll(unprocessedMsgs)

            // Prune tree
            patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, mTree) =>
              (joinPat, mTree.pruneTree(candidateQidxs.head._2))
            }

      if result.isEmpty then messages.append(q.take())
    result.get

}
