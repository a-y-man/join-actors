package join_patterns.matching.mutable

import join_actors.actor.ActorRef
import join_patterns.matching.{CandidateMatches, Matcher}
import join_patterns.types.JoinPattern

import java.util.concurrent.{Executors, LinkedTransferQueue as Mailbox}
import scala.collection.mutable.{ArrayBuffer, HashMap as MutableHashMap}
import scala.concurrent.duration.{Duration, HOURS}
import scala.concurrent.{Await, ExecutionContext, Promise}

class MutableStatefulTreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:

  private val messages = MutableHashMap[Int, M]()
  private var nextMessageIndex = 0

  private val matchingTrees: List[MutableMatchingTree[M, T]] =
    patterns.zipWithIndex.map(MutableMatchingTree(_, _))


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
