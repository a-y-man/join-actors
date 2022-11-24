package join_patterns

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

import java.util.concurrent.LinkedTransferQueue as Queue


case class MatchError(msg : String)

class Matcher[M, T](val patterns: List[JoinPattern[M, T]]) {
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages = ListBuffer[M]()

  private def compareIndcies(i1 : List[Int], i2: List[Int]) : Boolean =
    import math.Ordered.orderingToOrdered
    (i1.sorted < i2.sorted) || ((i1.sorted == i2.sorted) && (i1 < i2))

  def apply(q: Queue[M]): Either[T, MatchError] =
    import collection.convert.ImplicitConversions._

    val idxsI = ListBuffer[List[Int]]()
    val index = MutMap[List[Int], List[M]]()
    val patternVector : Map[List[M], T] =
      patterns.foldLeft(Map[List[M], T]()) {
        (patternVector, pattern) => // ASK: tuple accumulator
          if messages.isEmpty then
            messages.append(q.take())
            q.drainTo(messages)

          val (patternVectorEntry, idxMsgs, substs) = pattern.extract(messages.toList)
          val (msgIdxsQ, msgPattern) = idxMsgs.unzip
          idxsI.addOne(msgIdxsQ)
          index(msgIdxsQ) = (msgPattern)
          val isGuardTrue =
            if substs.isEmpty then true
            else pattern.guard(substs)

          if (messages.size >= pattern.size) && isGuardTrue then
            patternVectorEntry match
              case Some(entry) =>
                patternVector.updated(entry, pattern.rhs(substs))
              case None => patternVector
          else patternVector
      }

    println(s"I  = ${idxsI.mkString("; ")}")
    println(s"A = \nPattern\t\t -> \t RHS Value\n${patternVector.mkString("\n")}")

    val candidateMatches = idxsI.sortWith(compareIndcies).filter(!_.isEmpty)
    if !candidateMatches.forall(_.isEmpty) then
      println(s"I' = ${candidateMatches.mkString("; ")}\n")
      val selectedMatch = index(candidateMatches.head)
      val _match : Option[T] = patternVector.get(selectedMatch)
      _match match
        case Some(result) => Left(result)
        case None => Right(MatchError(s"No match!"))
    else
      Right(MatchError(s"No candidate matches for the messages {${messages.mkString("; ")}}"))

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