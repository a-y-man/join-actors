package join_patterns.matching.array_while

import join_actors.actor.ActorRef
import join_patterns.matching.{CandidateMatchOpt, CandidateMatches, Matcher}
import join_patterns.types.JoinPattern
import join_patterns.util.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.{ArrayBuffer, HashMap as MutableHashMap}

class ArrayWhileMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:

  private val messages = MutableHashMap[Int, M]()
  private var nextMessageIndex = 0

  private val matchingArrays: List[WhileMatchingArray[M, T]] =
    patterns.zipWithIndex.map(WhileMatchingArray(_, _))


  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    while result.isEmpty do
      val msg = q.take()
//      println(s"Received message $msg")
      val index = nextMessageIndex
      nextMessageIndex += 1

      messages.update(index, msg)

      val matches = ArrayBuffer[CandidateMatchOpt[M, T]]()
      for arr <- matchingArrays.fast do matches.append(arr.findMatch(index, msg, messages))

      val candidateMatches: CandidateMatches[M, T] =
        matches.foldLeft(CandidateMatches[M, T]()) {
          case (acc, Some(candidateMatch)) =>
            val (msgIdxs, p) = candidateMatch
            acc.updated(msgIdxs, p)
          case (acc, None) => acc
        }

      if candidateMatches.nonEmpty then
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs, selfRef))

        // Prune tree
        for tree <- matchingArrays.fast do
          tree.pruneTree(candidateQidxs)

        // Remove selected message indices from messages
        for idx <- candidateQidxs.fast do
          messages.remove(idx)

    result.get
