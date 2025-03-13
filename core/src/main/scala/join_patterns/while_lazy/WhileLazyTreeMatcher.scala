package join_patterns.while_lazy

import join_actors.actor.ActorRef
import join_patterns.matcher.{CandidateMatch, CandidateMatches, Matcher}
import join_patterns.types.JoinPattern
import join_patterns.util.*

import java.util.concurrent.{Executors, LinkedTransferQueue as Mailbox}
import scala.collection.mutable.{ArrayBuffer, HashMap as MutableHashMap}
import scala.concurrent.duration.{Duration, HOURS}
import scala.concurrent.{Await, ExecutionContext, Promise}

class WhileLazyTreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:

  private val messages = MutableHashMap[Int, M]()
  private var nextMessageIndex = 0

  private val matchingTrees: List[WhileLazyMatchingTree[M, T]] =
    patterns.zipWithIndex.map(WhileLazyMatchingTree(_, _))


  private val ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )


  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    while result.isEmpty do
      val msg = q.take()
//      println(s"Received message $msg")
      val index = nextMessageIndex
      nextMessageIndex += 1

      messages.update(index, msg)

//      val promises = ArrayBuffer[Promise[CandidateMatch[M, T]]]()
//      for tree <- matchingTrees do
//        val prom = Promise[CandidateMatch[M, T]]()
//        promises.append(prom)
//        ec.execute(() => prom.success(tree.findMatch(index, msg, messages)))
//
//      val matches = promises.map{ p => Await.result(p.future, Duration(1, HOURS)) }

      val matches = ArrayBuffer[CandidateMatch[M, T]]()
      for tree <- matchingTrees.fast do matches.append(tree.findMatch(index, msg, messages))

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
        for tree <- matchingTrees.fast do
          tree.pruneTree(candidateQidxs)

        // Remove selected message indices from messages
        for idx <- candidateQidxs.fast do
          messages.remove(idx)

    result.get
