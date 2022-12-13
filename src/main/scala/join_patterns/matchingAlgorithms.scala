package join_patterns

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import java.util.concurrent.LinkedTransferQueue as Queue


case class MatchError(msg : String)

// type ActivatedPatterns[M, T] = Map[(List[M], Map[String, Any]), Map[String, Any] => T]
// type Index[M] = Map[List[Int], (List[M], Map[String, Any])]

class Matcher[M, T](val patterns: List[JoinPattern[M, T]]) {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages = ListBuffer[M]()

  private def compareIndices(i1 : List[Int], i2: List[Int]) : Boolean =
    import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}
    (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

  def getActivatedPatterns[M, T](messages: ListBuffer[M]) : Map[(List[M], Map[String, Any]), Map[String, Any] => T] = ???
  def apply(q: Queue[M]): T =
    import collection.convert.ImplicitConversions._

    var result : Option[T] = None

    while result.isEmpty do
      if messages.isEmpty then
      messages.append(q.take())
      q.drainTo(messages)
      val idxsI = ListBuffer[List[Int]]()
      val index = MutMap[List[Int], (List[M], Map[String, Any])]()
      val activatedPatterns : Map[(List[M], Map[String, Any]), Map[String, Any] => T] =
        patterns.foldLeft(Map[(List[M], Map[String, Any]), Map[String, Any] => T]()) {
          (activatedPattern, pattern) =>
            val (activatedPatternsEntry, idxMsgs, substs) = pattern.extract(messages.toList)

            val (msgIdxsQ, msgPattern) = idxMsgs.unzip

            if !msgIdxsQ.isEmpty then
              idxsI.addOne(msgIdxsQ)
              index(msgIdxsQ) = (msgPattern, substs)

            val isGuardTrue =
              if substs.isEmpty then true
              else pattern.guard(substs)

            if (messages.size >= pattern.size) && isGuardTrue then
              activatedPatternsEntry match
                case Some(entry) =>
                  activatedPattern.updated((entry, substs), (subs : Map[String, Any]) => pattern.rhs(subs))
                case None => activatedPattern
            else activatedPattern
        }

      println(s"I  = ${idxsI.mkString("; ")}")
      println(s"A = \n(Pattern, Substitutions)\t\t -> \t RHS Closure\n${activatedPatterns.mkString("\n")}")

      val candidateMatches = idxsI.filter(!_.isEmpty).sortWith(compareIndices)
      if !candidateMatches.forall(_.isEmpty) then
        println(s"I' = ${candidateMatches.mkString("; ")}\n")
        val selectedMatch @ (msgs, subs) = index(candidateMatches.head)
        messages.subtractAll(msgs)
        val _match : Option[Map[String, Any] => T] = activatedPatterns.get(selectedMatch)
        _match match
          case Some(rhsFn) => result = Some(rhsFn(subs))
          case None => result = None


      if result.isEmpty then
        messages.append(q.take())
        q.drainTo(messages)

    result.get

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