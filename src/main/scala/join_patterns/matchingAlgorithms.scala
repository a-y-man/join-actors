package join_patterns

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import java.util.concurrent.LinkedTransferQueue as Queue
import java.util.concurrent.TimeUnit
import java.util.regex.Pattern

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

class Matcher[M, T](val patterns: List[JoinPattern[M, T]]) {
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

type PatternVector[M, T] = Map[List[Int], List[(Map[String, Any], Map[String, Any] => T)]]
object PatternVector:
  def apply[M, T](): PatternVector[M, T] =
    Map[List[Int], List[(Map[String, Any], Map[String, Any] => T)]]()

def printVector[M, T](patternVector: PatternVector[M, T]): Unit =
  patternVector.foreach { (k, v) =>
    val kToStr = s"${k.mkString("[", ", ", "]")}"
    val vToStr = v
      .map((fields, _) =>
        s"(${fields.map((k, v) => s"${k} -> ${v}").mkString("Map(", " , ", ")")}, RHS-closure)"
      )
      .mkString("{ ", ", ", " }")
    val mToStr = s"${kToStr}\t -> ${vToStr}"
    println(mToStr)
  }

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) {
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
      println("Msgs = " + messages.mkString("[", "; ", "]"))
      if messages.isEmpty then
        messages.append(q.take())

      val activatedPatterns: PatternVector[M, T] =
        patternsWithMatchingTrees.foldLeft(PatternVector[M, T]()) {
          (patternVector, patternWithMatchingTree) =>
            val ((pattern, patternIdx), mTree) = patternWithMatchingTree
            val updatedMatchingTree            = pattern.partialExtract(messages.toList, mTree)

            updatedPatternsWithMatchingTrees.addOne(
              ((pattern, patternIdx), updatedMatchingTree.get)
            )

            updatedMatchingTree match
              case Some(mTree) =>
                println("-------------------------------------------------------")
                println(s"Pattern Idx ${patternIdx}")
                printMapping(mTree.nodeMapping)
                println("-------------------------------------------------------")

                val enoughMsgToMatch =
                  mTree.nodeMapping.view.filterKeys(node => node.size >= pattern.size).toMap

                val fitToPattern = enoughMsgToMatch
                  .view.mapValues(candidateMatches =>
                    candidateMatches.filter((idxs, fields) => idxs.size == pattern.size)
                  )
                  .toMap
                  .filter((k, v) => v.nonEmpty)

                println("***********************************************************")
                println(s"Pattern Idx ${patternIdx}")
                printMapping(fitToPattern)
                println("***********************************************************")

                val whereGuardTrue = fitToPattern
                  .view.mapValues { candidateMatches =>
                    val trueGuardCandidates =
                      candidateMatches.filter((idxs, substs) => pattern.guard(substs))
                    trueGuardCandidates
                  }
                  .toMap
                  .filter((k, v) => v.nonEmpty)

                val withRhs = whereGuardTrue.map { (msgIdxs, candidateMatches) =>
                  (
                    msgIdxs,
                    candidateMatches.map { (_, substs) =>
                      (substs, (subs: Map[String, Any]) => pattern.rhs(substs))
                    }.toList
                  )
                }

                patternVector ++ withRhs

              case None => patternVector
        }

      patternsWithMatchingTrees = updatedPatternsWithMatchingTrees.toList
      println("Vector")
      printVector(activatedPatterns)

      if activatedPatterns.nonEmpty then
        val candidateQidxs = activatedPatterns.keys.toList.sortWith(compareQIndices)

        val candidateRHS = activatedPatterns.get(candidateQidxs.head).get

        if candidateRHS.nonEmpty then
          val (subst, rhsFn) = candidateRHS.head
          result = Some(rhsFn(subst))
          println(s"Q = ${messages.mkString("[", "; ", "]")}")
          // candidateQidxs.head.foreach {
          //   i => messages.remove(i)
          // }
          println(s"Q = ${messages.mkString("[", "; ", "]")}")

      if result.isEmpty then messages.append(q.take())

    result.get

}

// Msgs from Q     | Pattern Idxs from Pattern case A(x) & B() & A()

// Q = [(A(), 0)]
// [0]	 -> { [0], [2] }
// []	 -> {  }

// Q = [(A(), 0), (B(), 1)]
// [ []           -> { } ]
// [ [0]          -> { [0], [2] }]
// [ [0, 1]       -> { [0, 1], [2, 1] }]
// [ [1]          -> { [1] }]

//        0      1      2             0      1     2
// Q = [A(42), B(21), A(84)]       | A(x) & B(y) & A(z)
// Msgs from Q                     | Pattern Idxs from Pattern case
// [ Ø                     -> {} ]
// [ [1]                   -> { ([1], Map(y -> 21)) }]
// [ [0]                   -> { ([0], Map(x -> 42)), ([2], Map(z -> 42)) }]
// [ [2]	                 -> { ([0], Map(x -> 84)), ([2], Map(z -> 84)) }]
// [ [0, 1]                -> { ([0, 1], Map(x -> 42, y -> 21)), ([2, 1], Map(z -> 42, y -> 21)) }]
// [ [0, 2]	               -> { ([0, 2], Map(x -> 42, z -> 84)), ([2, 0], Map(z -> 42, x -> 84)) }]
// [ [1, 2]	               -> { ([0, 1], Map(x -> 42, y -> 21)), ([2, 1], Map(z -> 42, y -> 21)) }]
// [ [0, 1, 2]	           -> { ([0, 1, 2], Map(x -> 42, y -> 21, z -> 84)), ([2, 1, 0], Map(z -> 42, y -> 21, x -> 84)) }]

// Q = [(A(), 0), (B(), 1), (A(), 2)]
// []	         -> {  }
// [0]	       -> { [0], [2] }
// [1]	       -> { [1] }
// [2]	       -> { [0], [2] }
// [3]	       -> {  }
// [0, 1]	     -> { [1, 0], [1, 2] }
// [1, 2]	     -> { [0, 1], [2, 1] }
// [0, 2]	     -> { [0, 2], [2, 0] }
// [0, 1, 2]	 -> { [0, 1, 2], [2, 1, 0] }

// Q = [(C(), 0), (A(), 1), (B(), 2), (A(), 3)]
// []	           -> {  }
// [0]	         -> {  }
// [1]	         -> { [0], [2] }
// [2]	         -> { [1] }
// [3]	         -> { [0], [2] }
// [0, 3]	       -> {  }
// [2, 3]	       -> { [0, 1], [2, 1] }
// [0, 1]	       -> {  }
// [1, 2]	       -> { [1, 0], [1, 2] }
// [0, 2]	       -> {  }
// [1, 3]	       -> { [0, 2], [2, 0] }
// [0, 1, 2]	   -> {  }
// [0, 1, 3]	   -> {  }
// [0, 2, 3]	   -> {  }
// [1, 2, 3]	   -> { [0, 1, 2], [2, 1, 0] }
// [0, 1, 2, 3]	 -> {  }
