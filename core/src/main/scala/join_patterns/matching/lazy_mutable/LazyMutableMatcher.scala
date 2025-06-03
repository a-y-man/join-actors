package join_patterns.matching.lazy_mutable

import join_actors.actor.ActorRef
import join_patterns.matching.{CandidateMatches, Matcher}
import join_patterns.types.JoinPattern

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.HashMap as MutableHashMap

class LazyMutableMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:

  private val messages = MutableHashMap[Int, M]()
  private var nextMessageIndex = 0

  private val matchingTrees: List[LazyMutableMatchingTree[M, T]] =
    patterns.zipWithIndex.map(LazyMutableMatchingTree(_, _))


  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    while result.isEmpty do
      val msg = q.take()
      val index = nextMessageIndex
      nextMessageIndex += 1

      messages.update(index, msg)

      val matches = matchingTrees.map(_.findMatch(index, msg, messages))

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
        for matchingTree <- matchingTrees do
          matchingTree.pruneTree(candidateQidxs)

        // Remove selected message indices from messages
        candidateQidxs.foreach { idx =>
          messages.remove(idx)
        }

    result.get
