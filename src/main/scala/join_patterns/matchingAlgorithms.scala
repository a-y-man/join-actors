package join_patterns

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import java.util.concurrent.LinkedTransferQueue as Queue
import java.util.concurrent.TimeUnit

type ActivatedPatterns[M, T] = Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]
object ActivatedPatterns:
  def apply[M, T](): ActivatedPatterns[M, T] =
    Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]()

type Index[M] = MutMap[(Int, List[Int]), (List[M], Map[String, Any])]
object Index:
  def apply[M](): Index[M] =
    MutMap[(Int, List[Int]), (List[M], Map[String, Any])]()

def compareIndices(i1WithPatIdx: (Int, List[Int]), i2WithPatIdx: (Int, List[Int])): Boolean =
  import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

  val (_, i1) = i1WithPatIdx
  val (_, i2) = i2WithPatIdx

  (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

trait Matcher[M, T]

object Matcher:
  def apply[M, T](algorithm: AlgorithmType, patterns: List[JoinPattern[M, T]]) : Matcher[M, T] =
    algorithm match
      case AlgorithmType.BasicAlgorithm => BasicMatcher(patterns)
      case AlgorithmType.TreeBasedAlgorithm => TreeMatcher(patterns)


class BasicMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // private def compareIndices(i1WithPatIdx : (Int, List[Int]), i2WithPatIdx: (Int, List[Int])) : Boolean =
  //   import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

  //   val (i1PatIdx, i1) = i1WithPatIdx
  //   val (i2PatIdx, i2) = i2WithPatIdx

  //   (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then
        messages.append(q.poll(2, TimeUnit.SECONDS)) // Wait two seconds
        q.drainTo(messages)

      val idxsI = ListBuffer[(Int, List[Int])]()
      val index = Index[M]()
      val activatedPatterns: ActivatedPatterns[M, T] =
        patternsWithIdxs.foldLeft(ActivatedPatterns[M, T]()) { (activatedPattern, patternWithIdx) =>
          val (pattern, patternIdx)                     = patternWithIdx
          val (activatedPatternsEntry, idxMsgs, substs) = pattern.extract(messages.toList)

          val (msgIdxsQ, msgPattern) = idxMsgs.unzip

          val isGuardTrue = if activatedPatternsEntry.nonEmpty then pattern.guard(substs) else false

          if !msgIdxsQ.isEmpty && isGuardTrue then
            idxsI.addOne((patternIdx, msgIdxsQ))
            index((patternIdx, msgIdxsQ)) = (msgPattern, substs)

          if (messages.size >= pattern.size) && isGuardTrue then
            activatedPatternsEntry match
              case Some(actPat) =>
                activatedPattern.updated(
                  (patternIdx, actPat, substs),
                  (subs: Map[String, Any]) => pattern.rhs(subs)
                )
              case None => activatedPattern
          else activatedPattern
        }

      // println(s"I  = ${idxsI.mkString("; ")}")
      // println(s"A = \n(Pattern, Substitutions)\t\t -> \t RHS Closure\n${activatedPatterns.mkString("\n")}")

      val candidateMatches = idxsI.filter((_, mIdxsQ) => !mIdxsQ.isEmpty).sortWith(compareIndices)
      // print(s"Candidate matches: \n${candidateMatches.mkString("; \n")}\n")
      val candidateMatchesIdxs = candidateMatches // .map(_._2)
      if !candidateMatchesIdxs.isEmpty then
        val (patternIdx, msgIdxQ) = candidateMatchesIdxs.head
        val (msgs, subs)          = index(candidateMatchesIdxs.head)
        val selectedMatch         = (patternIdx, msgs, subs)
        // print(s"Messages matched: ${msgs.mkString("; ")}\n")
        messages.subtractAll(msgs)
        // print(s"active patterns: \n${activatedPatterns.mkString("; \n")}\n")
        val _match: Option[Map[String, Any] => T] = activatedPatterns.get(selectedMatch)
        result = _match.flatMap(rhsFn => Some(rhsFn(subs)))

      if result.isEmpty then
        messages.append(q.poll(2, TimeUnit.SECONDS)) // Wait two seconds
        q.drainTo(messages)

    result.get

}

type CandidateMatches[M, T] = Map[List[Int], List[(Map[String, Any], Map[String, Any] => T)]]
object CandidateMatches:
  def apply[M, T](): CandidateMatches[M, T] =
    Map[List[Int], List[(Map[String, Any], Map[String, Any] => T)]]()

def printCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]): Unit =
  candidateMatches.foreach { (k, v) =>
    val kToStr = s"${k.mkString("[", ", ", "]")}"
    val vToStr = v
      .map((fields, _) =>
        s"(${fields.map((k, v) => s"${k} -> ${v}").mkString("Map(", " , ", ")")}, RHS-closure)"
      )
      .mkString("{ ", ", ", " }")
    val mToStr = s"${kToStr}\t -> ${vToStr}"
    println(mToStr)
  }

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  def compareQIndices(i1: List[Int], i2: List[Int]): Boolean =
    import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

    (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

  // Init patterns with empty MatchingTree and maintain across apply() calls
  var patternsWithMatchingTrees = patternsWithIdxs
    .map { patternsWithIdxs =>
      (patternsWithIdxs, MatchingTree())
    }

  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    val updatedPatternsWithMatchingTrees = ListBuffer[((JoinPattern[M, T], Int), MatchingTree)]()

    while result.isEmpty do
      if messages.isEmpty then
        messages.append(q.take())
        // println("Msgs = " + messages.mkString("[", "; ", "]"))

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
                  mTree.nodeMapping.view.filterKeys(node => node.size >= pattern.size).toMap

                val whereGuardTrue = enoughMsgsToMatch
                  .view.mapValues { candidateMatches =>
                    val trueGuardCandidates =
                      candidateMatches.filter((idxs, substs) => pattern.guard(substs))
                    trueGuardCandidates
                  }
                  .toMap
                  .filter((k, v) => v.nonEmpty)

                val selectedMatch = whereGuardTrue.map { (msgIdxs, candidateMatches) =>
                  (
                    msgIdxs,
                    candidateMatches.map { (_, substs) =>
                      (substs, (subs: Map[String, Any]) => pattern.rhs(substs))
                    }.toList
                  )
                }

                // println("-------------------------------------------------------")
                matches ++ selectedMatch

              case None => matches
        }

      patternsWithMatchingTrees = updatedPatternsWithMatchingTrees.toList
      // if candidateMatches.nonEmpty then
      //   println("*******************************************************")
      //   println("Candidate Matches")
      //   printCandidateMatches(candidateMatches)
      //   println("*******************************************************")

      if candidateMatches.nonEmpty then
        val candidateQidxs = candidateMatches.keys.toList.sortWith(compareQIndices)

        val candidateRHS = candidateMatches.get(candidateQidxs.head).get

        if candidateRHS.nonEmpty then
          val (subst, rhsFn) = candidateRHS.head
          result = Some(rhsFn(subst))

          // Prune tree
          patternsWithMatchingTrees = patternsWithMatchingTrees.map {
            (joinPat, mTree) =>
              (joinPat, mTree.pruneTree(candidateQidxs.head))
          }

          // patternsWithMatchingTrees.foreach {
          //   (jp, mTree) =>
          //     println("Pruned")
          //     printMapping(mTree.nodeMapping)
          // }

          // println(s"Q = ${q.toList.mkString("[", "; ", "]")}")
          // candidateQidxs.head.foreach {
          //   i => messages.remove(i)
          // }
          // println(s"Q = ${q.toList.mkString("[", "; ", "]")}")

      if result.isEmpty then
        // println("Result still empty")
        messages.append(q.take())
        // println("Msgs' = " + messages.mkString("[", "; ", "]"))


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