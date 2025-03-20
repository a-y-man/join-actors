package join_patterns.matching.eager_parallel

import join_actors.actor.ActorRef
import join_patterns.matching.CandidateMatch
import join_patterns.matching.functions.*
import join_patterns.types.{JoinPattern, LookupEnv, MessageIdxs, PatternBins, PatternIdxs, given}
import join_patterns.util.*

import java.util.concurrent.{ConcurrentLinkedDeque, Executors, ThreadFactory}
import java.util.TreeMap as JavaTreeMap
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map as MutableMap, TreeMap as MutableTreeMap, ArrayDeque}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import scala.concurrent.{Await, ExecutionContext, Promise}

class EagerParallelMatchingTree[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int):
  private val patternExtractors = pattern.getPatternInfo.patternExtractors

  private val nodes = JavaTreeMap[MessageIdxs, PatternBins](sizeBiasedOrdering)
  nodes.put(MessageIdxs(), pattern.getPatternInfo.patternBins)

  private val NUM_THREADS = 4
  private val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(NUM_THREADS, Thread.ofVirtual().factory())
    )

  private def updateTree(newMessageIdx: Int, msg: M): MutableMap[MessageIdxs, PatternBins] =
    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, (msgTypeChecker, _msgFieldExtractor)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(PatternIdxs)

    if matchingConstructorIdxs.isEmpty then MutableTreeMap()
    else
      val additions = ConcurrentLinkedDeque[(MessageIdxs, PatternBins)]()
      val completePatterns = ConcurrentLinkedDeque[(MessageIdxs, PatternBins)]()
      val promises = ArrayBuffer[Promise[Unit]]()

      val treeSize = nodes.size
      val q = ArrayDeque(nodes.entrySet().spliterator())
      var exhausted = false
      while q.length < NUM_THREADS && !exhausted do
        val spliterator = q.removeLast(false)
        val newSpliterator = spliterator.trySplit()

        if newSpliterator == null then
          q.prepend(spliterator)
          exhausted = true
        else
          q.prepend(spliterator)
          q.prepend(newSpliterator)

      while q.nonEmpty do
        val promise = Promise[Unit]()
        promises.append(promise)

        val range = q.removeLast(false)
        executionContext.execute: () =>
          range.forEachRemaining: entry =>
            val messageIdxsMatched = entry.getKey
            val bins = entry.getValue
            // Create the child for one leaf in the matching tree

            // If the PatternBins contains a key for the constructor type of the new message, we might be able to add a child
            bins.get(matchingConstructorIdxs) match
              case None => ()
              case Some(mappedMessageIdxs) =>
                // We only add a new node if some of the constructor instances in the pattern don't already have a match
                if mappedMessageIdxs.size < matchingConstructorIdxs.size then
                  val newMessageIdxs = messageIdxsMatched :+ newMessageIdx
                  val newPatternBins = bins.updated(matchingConstructorIdxs, mappedMessageIdxs :+ newMessageIdx)
                  val newNode = (newMessageIdxs, newPatternBins)
                  additions.add(newNode)

                  if newMessageIdxs.size == pattern.size
                    && newPatternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
                  then completePatterns.add(newNode)

          promise.success(())

      for promise <- promises.fast do Await.ready(promise.future, Duration.Inf)
      nodes.asScala.addAll(additions.iterator().asScala)

      completePatterns.iterator().asScala.to(MutableTreeMap)

  def findMatch(index: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatch[M, T] =
    val completePatterns = updateTree(index, msg)

    if completePatterns.isEmpty then return None

    val possibleMatches = completePatterns.iterator
      .map: (msgIdxs, patternBins) =>
        val validPermutations =
          getMsgIdxsWithPayloadExtractor(patternExtractors, patternBins)
        val bestMatchOpt = findFairestMatch(validPermutations, messages, pattern)
        bestMatchOpt
      .collectFirst { case Some(_bestMatch) => _bestMatch }

    possibleMatches match
      case Some((bestMatchIdxs, bestMatchSubsts)) =>
        val selectedMatch =
          (
            bestMatchSubsts,
            (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
          )

        completePatterns.remove(bestMatchIdxs)
        nodes.asScala.subtractAll(completePatterns.keySet)

        Some((bestMatchIdxs, patternIdx), selectedMatch)
      case None =>
        for k <- completePatterns.keySet do
          nodes.remove(k)

        None

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    nodes.asScala.filterInPlace: (messageIdxs, _) =>
      messageIdxsToRemove.forall(i => !messageIdxs.contains(i))
