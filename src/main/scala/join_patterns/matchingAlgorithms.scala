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


class MatcherTree[M, T](val patterns: List[JoinPattern[M, T]], val cases : List[M]) {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages = ListBuffer[M]()

  private def compareIndices(i1 : List[Int], i2: List[Int]) : Boolean =
    import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}
    (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

  private def getIndex(msgs : List[M]) : List[Int] =
    msgs.map(m => cases.indexOf(m))
}

// class NaiveMatcher[M, T](val patterns: List[JoinPattern[M, T]]) {
//   // Messages extracted from the queue are saved here to survive across apply() calls
//   private val messages = ListBuffer[M]()

//   def apply(q: Queue[M]): T =
//     import collection.convert.ImplicitConversions._

//     var result: Option[T] = None

//     while (result.isEmpty)
//       for
//         pattern <- patterns
//         if result.isEmpty
//       do
//         if messages.size >= pattern.size then
//           val (matchedMessages, substs) = pattern.extract(messages.toList)

//           if matchedMessages.nonEmpty && pattern.guard(substs) then
//             result = Some(pattern.rhs(substs))
//             messages.subtractAll(matchedMessages)

//       if result.isEmpty then
//         // If we arrive here, no pattern has been matched and we need more msgs
//         messages.append(q.take())
//         q.drainTo(messages)

//     result.get
// }