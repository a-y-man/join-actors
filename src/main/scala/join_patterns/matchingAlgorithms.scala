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

def _compareIndices(i1WithPatIdx: (List[Int], Int), i2WithPatIdx: (List[Int], Int)): Boolean =
  import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

  val (i1, _) = i1WithPatIdx
  val (i2, _) = i2WithPatIdx

  val i1sorted = i1.sorted
  val i2sorted = i2.sorted

  (i1sorted < i2sorted) || ((i1sorted == i2sorted) && (i1 < i2))

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
      println(s"Candidate idxs\n${candidateIndexes.mkString("\n")}")
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
    TreeMap[(List[Int], Int), (Map[String, Any], Map[String, Any] => T)]
  object CandidateMatches:
    def apply[M, T](): CandidateMatches[M, T] =
      TreeMap[(List[Int], Int), (Map[String, Any], Map[String, Any] => T)]()(
        Ordering[(List[Int], Int)]
      )

  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls
  var initMatchingTree = MatchingTree[M](nodeMapping = NodeMapping[M]())
  var patternsWithMatchingTrees = patternsWithIdxs
    .map { patternsWithIdxs =>
      (patternsWithIdxs, initMatchingTree)
    }
  def computeCombinations(lists: List[(Int, List[Int])]): LazyList[List[(Int, Int)]] = {
    def generateCombinations(lists: List[(Int, List[Int])]): LazyList[List[(Int, Int)]] =
      lists match {
        case Nil => LazyList(List.empty[(Int, Int)])
        case (msgIdx, head) :: tail =>
          for {
            elem        <- head.to(LazyList)
            combination <- generateCombinations(tail)
            if !combination.exists((_, patIdx) => elem == patIdx)
          } yield (msgIdx, elem) :: combination
      }

    generateCombinations(lists)
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

              val enoughMsgsToMatch
                  : Option[(List[Int], Set[(Int, M => Boolean, M => Map[String, Any])])] =
                mTree.nodeMapping.view.find((node, fits) =>
                  node.size >= pattern.size && fits.nonEmpty
                )

              if enoughMsgsToMatch.nonEmpty then
                val msgIdxsQ = enoughMsgsToMatch.get._1
                val msgIdxsToFits =
                  enoughMsgsToMatch.map { (msgIdxs, fits) =>
                    msgIdxs.foldLeft(Map[Int, Set[(Int, M => Boolean, M => Map[String, Any])]]()) {
                      (msgIdxsToFits, msgIdx) =>
                        val m = messages(msgIdx)
                        val msgIdxToFits = fits.filter { (patIdx, isMsg, fieldExtractor) =>
                          isMsg(m)
                        }
                        msgIdxsToFits.updated(msgIdx, msgIdxToFits)
                    }
                  }.get

                msgIdxsToFits.foreach { (k, v) =>
                  val fits =
                    v.map { (p, i, f) => s"(${p}, i-CLOSURE, f-CLOSURE)" }.mkString("[", "; ", "]")
                  println(s"${k} -> ${fits}")
                }

                val simplifiedFits = msgIdxsToFits.map { (msgIdx, fits) =>
                  val simpleFits = fits.map((p, i, f) => p)
                  // println(s"Simplified fits\n${msgIdx} -> ${simpleFits}")
                  (msgIdx, simpleFits.toList)
                }.toList

                val possibleFits = computeCombinations(simplifiedFits)

                println(s"Possible fits ${possibleFits.size}")

                def computeSubsts =
                  (possibleFit: List[(Int, Int)]) =>
                    possibleFit.foldLeft(Map[String, Any]()) { (substs, idxs) =>
                      val (msgIdx, patIdx) = idxs
                      val subs = msgIdxsToFits(msgIdx)
                        .find(_._1 == patIdx)
                        .map((_, _, extractField) => extractField(messages(msgIdx)))
                        .get
                      substs ++ subs
                    }

                val substsWhereGuardTrue =
                  possibleFits
                    .find { possibleFit =>
                      println(s"[DEBUG] TRYING!! ${patternIdx}")
                      val substs = computeSubsts(possibleFit)
                      pattern.guard(substs)
                    }
                    .map(computeSubsts) // FIXME: Redundant computation
                    .get

                println(s"subs ${substsWhereGuardTrue}")

                val selectedMatch =
                  (
                    substsWhereGuardTrue,
                    (subs: Map[String, Any]) => pattern.rhs(subs)
                  )

                (
                  _updatedMTs,
                  matches.updated((msgIdxsQ, patternIdx), selectedMatch)
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
          // println(s"[DEBUG]\n${candidateMatches.mkString("\n")}")
          val candidateQidxs = candidateMatches.head
          // println(s"Candidate idxs\n${candidateQidxs.mkString("\n")}")

          val candidateRHS = candidateMatches.get(candidateQidxs.head)
          if candidateRHS.nonEmpty then
            val (subst, rhsFn) = candidateRHS.head
            result = Some(rhsFn(subst))

            val unprocessedMsgs = messages.zipWithIndex
              .filterNot((_, idx) => candidateQidxs.head._1.contains(idx))
              .map(_._1)
            messages.clear()
            messages.addAll(unprocessedMsgs)

            // Prune tree
            patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, mTree) =>
              (joinPat, mTree.pruneTree(candidateQidxs.head._1))
            }

      if result.isEmpty then messages.append(q.take())
    result.get

}

// for {
//   possibleFit <- possibleFits
//   substs = possibleFit.foldLeft(Map[String, Any]()) { (substs, idxs) =>
//     val (msgIdx, patIdx) = idxs
//     val sub = msgIdxsToFits(msgIdx)
//       .find(elem => elem._1 == patIdx)
//       .map(elem => elem._3(messages(msgIdx)))
//       .get
//     substs ++ sub
//   }
//   if pattern.guard(substs)
// } yield substs
