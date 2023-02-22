package join_patterns

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import java.util.concurrent.LinkedTransferQueue as Queue
import java.util.concurrent.TimeUnit
import scala.annotation.meta.field

trait Matcher[M, T] {
  def apply(q: Queue[M]): T
}

object Matcher:
  def apply[M, T](algorithm: AlgorithmType, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case AlgorithmType.BasicAlgorithm     => BasicMatcher(patterns)
      case AlgorithmType.TreeBasedAlgorithm => TreeMatcher(patterns)

def compareIndices(i1WithPatIdx: (Int, List[Int]), i2WithPatIdx: (Int, List[Int])): Boolean =
  import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

  val (_, i1) = i1WithPatIdx
  val (_, i2) = i2WithPatIdx

  (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

class BasicMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Type aliases used only within this class
  type ActivatedPatterns[M, T] = Map[(Int, List[M], Map[String, Any]), Map[String, Any] => T]
  object ActivatedPatterns:
    def apply[M, T](): ActivatedPatterns[M, T] =
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

class TreeMatcher[M, T](val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T] {
  // Type alias used only within this class
  type CandidateMatches[M, T] =
    Map[(Int, List[Int]), (Map[String, Any], Map[String, Any] => T)]
  object CandidateMatches:
    def apply[M, T](): CandidateMatches[M, T] =
      Map[(Int, List[Int]), (Map[String, Any], Map[String, Any] => T)]()

  private def printCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]): Unit =
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

  def permutations[T](lst: List[T]): List[List[T]] = {
    if (lst.isEmpty) List(List())
    else {
      for {
        (x, i) <- lst.zipWithIndex
        perm <- permutations(lst.take(i) ++ lst.drop(i + 1))
      } yield x :: perm
    }
  }


  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result: Option[T] = None

    val updatedPatternsWithMatchingTrees = ListBuffer[((JoinPattern[M, T], Int), MatchingTree[M])]()

    while result.isEmpty do
      if messages.isEmpty then
        // println(s"Q = ${q.zipWithIndex.mkString("[", ", ", "]")}")
        messages.append(q.take())
        // println(s"Ms = ${messages.mkString("[", ", ", "]")}")

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
                println("-------------------------------------------------------")
                println(s"Pattern Idx ${patternIdx}")
                printMapping(mTree.nodeMapping.filter((_, fits) => fits.nonEmpty))

                val enoughMsgsToMatch =
                  mTree.nodeMapping.view
                    .filterKeys(node => node.size >= pattern.size)
                    .filter((_, m) => m.nonEmpty)
                    .toMap

                println("===================================================================")
                println("Enough Messages")
                printMapping(enoughMsgsToMatch)

                val possibleFits =
                  enoughMsgsToMatch.keySet.flatMap { msgIdxs =>
                    val mapping = msgIdxs.map { i =>
                      val res = enoughMsgsToMatch(msgIdxs)
                        .map { (patIdx, isMsg, fieldExtractor) =>
                          val m = messages(i)
                          val candidateMapping =
                            if isMsg(m) then Map(patIdx -> fieldExtractor(m))
                            else Map(-1                 -> Map.empty)
                          candidateMapping.filter((k, _) => k != -1)
                        }
                        .filter(_.nonEmpty)
                        .reduce(_ ++ _)
                      (i, res)
                    }
                    mapping
                  }.toList
                println(s"PossibleFits\n${possibleFits.mkString("\n")} ${possibleFits.hashCode()}")

                // 4 -> Map(1 -> Map(y -> 1)))
                // 3 -> Map(0 -> Map(x -> 3)), (2 -> Map(z -> 3)))
                // 5 -> Map(0 -> Map(x -> 2)), (2 -> Map(z -> 2)))

                // ([3,4,5], {0, 1, 2}, Map((x -> 3), (y -> 1), (z -> 2)))
                // ([5,4,3], {0, 1, 2}, Map((x -> 2), (y -> 1), (z -> 3)))
                // ([3,5,4], {0, 1, 2}, Map((x -> 3), (y -> 1), (z -> 2)))

                val candidatePerms = possibleFits.permutations
                val candi = candidatePerms.toList
                println(s"candi ${candi.size}")
                println(s"candi ${candi.size}")

                val matchesWithSubsts = candidatePerms
                  .map { perms =>
                    perms.foldLeft((List[Int](), Set[Int](), Map[String, Any]())) {
                      (acc, mapping) =>
                        val (msgIdx, elem)               = mapping
                        val (seenMsgs, seenPats, substs) = acc

                        val patIdxs = elem.keySet.toList.sorted

                        val k = patIdxs.find(k => !seenPats.contains(k)).get
                        (seenMsgs.appended(msgIdx), seenPats.`+`(k), substs ++ (elem(k)))
                    }
                  }
                  .map((msgIdxs, _, substs) => (msgIdxs, substs))

                // val matchesWithSubsts_ = List.newBuilder[(List[Int], Map[String, Any])]

                // while (matchesWithSubsts.hasNext) {
                //   val element = matchesWithSubsts.next()
                //   matchesWithSubsts_ += element
                // }

                // val result = matchesWithSubsts_.result()
                // val result1 = result.toList
                // val result2 = result1
                // result1.foreach(println)
                // println(s"RES ${result}")
                // println(s"RES ${result1}")
                // println(s"RES ${result2}")
                println(s"RES ${matchesWithSubsts.toList}")

                // matchesWithSubsts.foreach {
                //   (msgIdxs, substs) =>
                //     println(s"${msgIdxs.mkString("[", ", " ,"]")} ${pattern.guard(substs)}")
                // }

                println("===================================================================")
                val whereGuardTrue = matchesWithSubsts
                  .filter((idxs, substs) => pattern.guard(substs))
                  .filter((k, v) => v.nonEmpty)
                // println(s"RES ${matchesWithSubsts.toList}")
                // println(s"WhereGuardTrue ${whereGuardTrue.toList}")

                val selectedMatch = whereGuardTrue.map { (msgIdxs, substs) =>
                  (
                    (patternIdx, msgIdxs),
                    (substs, (subs: Map[String, Any]) => pattern.rhs(subs))
                  )
                }.toList

                println(s"Selected ${selectedMatch}")

                println("-------------------------------------------------------")
                matches ++ selectedMatch

              case None => matches
        }

      patternsWithMatchingTrees = updatedPatternsWithMatchingTrees.toList
      if candidateMatches.nonEmpty then
        println("*******************************************************")
        println("Candidate Matches")
        printCandidateMatches(candidateMatches)
        println("*******************************************************")

        if candidateMatches.nonEmpty then
          val candidateQidxs = candidateMatches.keys.toList.sortWith(compareIndices)
          val candidateRHS   = candidateMatches.get(candidateQidxs.head)

          if candidateRHS.nonEmpty then
            val (subst, rhsFn) = candidateRHS.head
            result = Some(rhsFn(subst))

            // Prune tree
            // patternsWithMatchingTrees = patternsWithMatchingTrees.map { (joinPat, mTree) =>
            //   (joinPat, mTree.pruneTree(candidateQidxs.head._2))
            // }

      if result.isEmpty then
        // println(s"Q = ${q.zipWithIndex.mkString("[", ", ", "]")}")
        messages.append(q.take())
        // println(s"M = ${messages.mkString("[", ", ", "]")}")

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
