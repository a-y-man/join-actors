package join_patterns

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import java.util.concurrent.LinkedTransferQueue as Queue
import java.util.concurrent.TimeUnit

type ActivatedPatterns[M, T] = Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]
object ActivatedPatterns:
  def apply[M, T]() : ActivatedPatterns[M, T] =
    Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]()

type Index[M] = MutMap[(Int, List[Int]), (List[M], Map[String, Any])]
object Index:
  def apply[M]() : Index[M] =
    MutMap[(Int, List[Int]), (List[M], Map[String, Any])]()

class Matcher[M, T](val patterns: List[JoinPattern[M, T]]) {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  private def compareIndices(i1WithPatIdx : (Int, List[Int]), i2WithPatIdx: (Int, List[Int])) : Boolean =
    import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

    val (i1PatIdx, i1) = i1WithPatIdx
    val (i2PatIdx, i2) = i2WithPatIdx

    (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))


  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result : Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then
        messages.append(q.poll(2, TimeUnit.SECONDS)) // Wait two seconds
        q.drainTo(messages)

      val idxsI = ListBuffer[(Int, List[Int])]()
      val index = Index[M]()
      val activatedPatterns : ActivatedPatterns[M, T] =
        patternsWithIdxs.foldLeft(ActivatedPatterns[M, T]()) {
          (activatedPattern, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            val (activatedPatternsEntry, idxMsgs, substs) = pattern.extract(messages.toList)

            val (msgIdxsQ, msgPattern) = idxMsgs.unzip

            val isGuardTrue = pattern.guard(substs)

            if !msgIdxsQ.isEmpty && isGuardTrue then
              idxsI.addOne((patternIdx, msgIdxsQ))
              index((patternIdx, msgIdxsQ)) = (msgPattern, substs)

            if (messages.size >= pattern.size) && isGuardTrue then
              activatedPatternsEntry match
                case Some(actPat) =>
                  activatedPattern.updated((patternIdx, actPat, substs), (subs : Map[String, Any]) => pattern.rhs(subs))
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
        val (msgs, subs) = index(candidateMatchesIdxs.head)
        val selectedMatch = (patternIdx, msgs, subs)
        // print(s"Messages matched: ${msgs.mkString("; ")}\n")
        messages.subtractAll(msgs)
        // print(s"active patterns: \n${activatedPatterns.mkString("; \n")}\n")
        val _match : Option[Map[String, Any] => T] = activatedPatterns.get(selectedMatch)
        result = _match.flatMap(rhsFn => Some(rhsFn(subs)))

      if result.isEmpty then
        messages.append(q.poll(2, TimeUnit.SECONDS)) // Wait two seconds
        q.drainTo(messages)

    result.get

}

// //                 Msgs from Q     | Pattern Idxs from Pattern case
// //                 [ Ø            -> {} ]
// //                 [ {1}          -> { [1, 3] }]
// //                 [ {1, 2}       -> { [1, 2], [3, 2] }]
// //                 [ {2}          -> { [2] }]
// type NodeMapping = Map[Set[Int], Set[List[Int]]]
// object NodeMapping:
//   def apply() : Map[Set[Int], Set[List[Int]]] =
//     Map[Set[Int], Set[List[Int]]]()

// // Edges
// //               { (Ø, {1}), ({1}, {[1, 3]}), ({1, 2},  ) }
// type TreeEdges = Set[Tuple2[Set[Int], Set[Int]]]
// object TreeEdges:
//   def apply() : Set[Tuple2[Set[Int], Set[Int]]] =
//     Set[Tuple2[Set[Int], Set[Int]]]()


// case class MatchingTree[M](
//   nodeMapping : NodeMapping,
//   treeEdges : TreeEdges
// )

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  private def compareIndices(i1WithPatIdx : (Int, List[Int]), i2WithPatIdx: (Int, List[Int])) : Boolean =
    import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

    val (_, i1) = i1WithPatIdx
    val (_, i2) = i2WithPatIdx

    (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))


  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result : Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then
        messages.append(q.poll(2, TimeUnit.SECONDS)) // Wait two seconds
        q.drainTo(messages)








      if result.isEmpty then
        messages.append(q.poll(2, TimeUnit.SECONDS)) // Wait two seconds
        q.drainTo(messages)

    result.get



}