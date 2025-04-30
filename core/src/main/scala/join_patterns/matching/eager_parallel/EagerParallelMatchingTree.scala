package join_patterns.matching.eager_parallel

import join_actors.actor.ActorRef
import join_patterns.matching.CandidateMatchOpt
import join_patterns.matching.functions.*
import join_patterns.types.{*, given}
import join_patterns.util.*

import java.util.concurrent.{ConcurrentLinkedDeque, Executors, ThreadFactory}
import java.util.{Map, Spliterator, TreeMap as JavaTreeMap}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayDeque, Map as MutableMap, TreeMap as MutableTreeMap}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import scala.concurrent.{Await, ExecutionContext, Promise}

class EagerParallelMatchingTree[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int, private val numThreads: Int):
  private val patternExtractors = pattern.getPatternInfo.patternExtractors

  private val nodes = JavaTreeMap[MessageIdxs, PatternBins](sizeBiasedOrdering)
  nodes.put(MessageIdxs(), pattern.getPatternInfo.patternBins)

  private type Node = (MessageIdxs, PatternBins)
  private type IterResult = (ArrayBuffer[Node], ArrayBuffer[Node])

  private val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(numThreads)
    )

  private def updateTree(newMessageIdx: Int, msg: M): MutableMap[MessageIdxs, PatternBins] =
    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, PatternIdxInfo(msgTypeChecker, _msgFieldExtractor, _)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(PatternIdxs)

    if matchingConstructorIdxs.isEmpty then MutableTreeMap()
    else
      val promises = ArrayBuffer[Promise[IterResult]]()

      val q = divideSpliterator(nodes.entrySet().spliterator(), numThreads)

      while q.nonEmpty do
        val promise = Promise[IterResult]()
        promises.append(promise)

        val range = q.removeHead(false)
        executionContext.execute: () =>
          val additions = ArrayBuffer[Node]()
          val completePatterns = ArrayBuffer[Node]()

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
                  additions.append(newNode)

                  if newMessageIdxs.size == pattern.size
                    && newPatternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
                  then completePatterns.append(newNode)

          promise.success((additions, completePatterns))

      val finalCompletePatterns = MutableTreeMap[MessageIdxs, PatternBins]()

      for promise <- promises.fast do Await.ready(promise.future, Duration.Inf)
      for promise <- promises.fast do
        val (additions, completePatterns) = Await.result(promise.future, Duration.Inf)
        nodes.asScala.addAll(additions)
        finalCompletePatterns.addAll(completePatterns)

      finalCompletePatterns

  def findMatch(index: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =
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
        for k <- completePatterns.keySet.fast do
          nodes.remove(k)

        None

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    nodes.keySet().removeIf: messageIdxs =>
      messageIdxsToRemove.exists(i => messageIdxs.contains(i))
