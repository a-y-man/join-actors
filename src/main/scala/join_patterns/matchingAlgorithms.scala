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
      if messages.isEmpty then
        messages.append(q.take())
        q.drainTo(messages)

      val index = Index[M]()
      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx)             = patternWithIdx
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

      // println(s"I  = ${idxsI.mkString("; ")}")
      // println(s"A = \n(Pattern, Substitutions)\t\t -> \t RHS Closure\n${activatedPatterns.mkString("\n")}")
      // print(s"Messages matched: ${msgs.mkString("; ")}\n")
      // print(s"active patterns: \n${activatedPatterns.mkString("; \n")}\n")

      val candidateIndexes = index.keys.toList.sortWith(compareIndices)
      // print(s"Candidate matches: \n${candidateIndexes.mkString("\n")}\n")
      if !candidateIndexes.isEmpty then
        val (patternIdx, msgIdxQ) = candidateIndexes.head
        val (msgs, subs)          = index(candidateIndexes.head)
        val selectedMatch         = (patternIdx, msgs, subs)

        result = for {
          rhsFn <- candidateMatches.get(selectedMatch)
        } yield rhsFn(subs)

        messages.subtractAll(msgs)

      if result.isEmpty then
        messages.append(q.take())
        q.drainTo(messages)

    result.get

}

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Type alias used only within this class
  type CandidateMatches[M, T] =
    TreeMap[(Int, List[Int]), (Map[String, Any], Map[String, Any] => T)]
  object CandidateMatches:
    def apply[M, T](): CandidateMatches[M, T] =
      TreeMap[(Int, List[Int]), (Map[String, Any], Map[String, Any] => T)]()

  def printCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]): Unit =
    candidateMatches.foreach { (k, v) =>
      val kToStr = s"(${k._1.toString()}, ${k._2.mkString("[", ", ", "]")})"
      val vToStr =
        val (fields, _) = v
        s"(${fields.map((k, v) => s"${k} -> ${v}").mkString("Map(", " , ", ")")}, RHS-closure)"
      // .mkString("{ ", ", ", " }")
      val mToStr = s"${kToStr}\t -> ${vToStr}"
      println(mToStr)
    }

  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls
  var initMatchingTree = MatchingTree[M](nodeMapping = NodeMapping[M](), treeEdges = TreeEdges())
  var patternsWithMatchingTrees = patternsWithIdxs
    .map { patternsWithIdxs =>
      (patternsWithIdxs, initMatchingTree)
    }

  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    val updatedPatternsWithMatchingTrees = ListBuffer[((JoinPattern[M, T], Int), MatchingTree[M])]()

    while result.isEmpty do
      if messages.isEmpty then
        // println(s"Q = ${q.zipWithIndex.mkString("[", ", ", "]")}")
        messages.append(q.poll(2, TimeUnit.SECONDS))
        // println(s"M = ${messages.mkString("[", ", ", "]")}")

      val candidateMatches: CandidateMatches[M, T] =
        patternsWithMatchingTrees.foldLeft(CandidateMatches[M, T]()) {
          (matches, patternWithMatchingTree) =>
            val ((pattern, patternIdx), mTree) = patternWithMatchingTree
            val updatedMatchingTree            = pattern.partialExtract(messages.toList, mTree)

            updatedPatternsWithMatchingTrees.addOne(
              ((pattern, patternIdx), updatedMatchingTree.get)
            )

            updatedMatchingTree match
              case Some(mTree) =>
                // println("-------------------------------------------------------")
                // println(s"Pattern Idx ${patternIdx}")
                // printMapping(mTree.nodeMapping)

                val enoughMsgsToMatch =
                  mTree.nodeMapping.view.find((node, fits) =>
                    node.size >= pattern.size && fits.nonEmpty
                  ) // .to(TreeMap)

                if enoughMsgsToMatch.nonEmpty then
                  // println("===================================================================")
                  // println("Enough Messages")
                  // printMapping(enoughMsgsToMatch.to(TreeMap))
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
                          val patIdx = possibleFitToSubsts.keySet.find(k => !seenPats.contains(k)).get
                          (seenMsgs :+ msgIdx, seenPats + patIdx, substs ++ possibleFitToSubsts(patIdx))
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

                  // println("-------------------------------------------------------")
                  matches ++ selectedMatch
                else
                  // println("-------------------------------------------------------")
                  matches

              case None => matches
        }

      patternsWithMatchingTrees = updatedPatternsWithMatchingTrees.toList
      if candidateMatches.nonEmpty then
        // println("*******************************************************")
        // println("Candidate Matches")
        // printCandidateMatches(candidateMatches)
        // println("*******************************************************")

        if candidateMatches.nonEmpty then
          val candidateQidxs = candidateMatches.keys.toList.sortWith(compareIndices)
          // println(s"${candidateQidxs.mkString("\n")}")

          val candidateRHS = candidateMatches.get(candidateQidxs.head)
          if candidateRHS.nonEmpty then
            val (subst, rhsFn) = candidateRHS.head
            result = Some(rhsFn(subst))

            val prunedMsgs = messages.zipWithIndex
              .filterNot((_, idx) => candidateQidxs.head._2.contains(idx))
              .map(_._1)
            messages.clear()
            messages.addAll(prunedMsgs)

            // Prune tree
            patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, mTree) =>
              (joinPat, mTree.pruneTree(candidateQidxs.head._2))
            }

      if result.isEmpty then
        // println(s"Q' = ${q.zipWithIndex.mkString("[", ", ", "]")}")
        messages.append(q.poll(2, TimeUnit.SECONDS))
        // println(s"M' = ${messages.mkString("[", ", ", "]")}")

    result.get

}

//        0      1      2             0      1     2
// Q = [A(42), B(21), A(84)]       | A(x) & B(y) & A(z)
// Msgs from Q                     | Pattern Idxs from Pattern case
// [ Ø                     -> {} ]
// [ [0]                   -> { ([0], Map(x -> 42)), ([2], Map(z -> 42)) }]
// [ [1]                   -> { ([1], Map(y -> 21)) }]
// [ [2]	                 -> { ([0], Map(x -> 84)), ([2], Map(z -> 84)) }]
// [ [0, 1]                -> { ([0, 1], Map(x -> 42, y -> 21)), ([2, 1], Map(z -> 42, y -> 21)) }]
// [ [0, 2]	               -> { ([0, 2], Map(x -> 42, z -> 84)), ([2, 0], Map(z -> 42, x -> 84)) }]
// [ [1, 2]	               -> { ([0, 1], Map(x -> 42, y -> 21)), ([2, 1], Map(z -> 42, y -> 21)) }]
// [ [0, 1, 2]	           -> { ([0, 1, 2], Map(x -> 42, y -> 21, z -> 84)), ([2, 1, 0], Map(z -> 42, y -> 21, x -> 84)) }]

// 4 -> Map(1 -> Map(y -> 1)))
// 3 -> Map(0 -> Map(x -> 3)), (2 -> Map(z -> 3)))
// 5 -> Map(0 -> Map(x -> 2)), (2 -> Map(z -> 2)))

// ([3,4,5], {0, 1, 2}, Map((x -> 3), (y -> 1), (z -> 2)))
// ([5,4,3], {0, 1, 2}, Map((x -> 2), (y -> 1), (z -> 3)))
// ([3,5,4], {0, 1, 2}, Map((x -> 3), (y -> 1), (z -> 2)))
