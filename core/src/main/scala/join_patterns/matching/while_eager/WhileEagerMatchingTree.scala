package join_patterns.matching.while_eager

import join_actors.actor.ActorRef
import join_patterns.matching.CandidateMatchOpt
import join_patterns.matching.functions.*
import join_patterns.types.{*, given}
import join_patterns.util.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Map as MutableMap, TreeMap as MutableTreeMap}

class WhileEagerMatchingTree[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int):
  private val patternExtractors = pattern.getPatternInfo.patternExtractors

  private val nodes = MutableTreeMap[MessageIdxs, PatternBins](MessageIdxs() -> pattern.getPatternInfo.patternBins)


  private def updateTree(newMessageIdx: Int, msg: M): MutableMap[MessageIdxs, PatternBins] =
    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, PatternIdxInfo(msgTypeChecker, _msgFieldExtractor, _)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(PatternIdxs)

    if matchingConstructorIdxs.isEmpty then MutableTreeMap()
    else
      val additions = ArrayBuffer[(MessageIdxs, PatternBins)]()
      val completePatterns = MutableTreeMap[MessageIdxs, PatternBins]()

      for (messageIdxsMatched, bins) <- nodes.fast do
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
              then completePatterns.addOne(newNode)

      nodes.addAll(additions)

      completePatterns

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
        nodes.subtractAll(completePatterns.keySet)

        Some((bestMatchIdxs, patternIdx), selectedMatch)
      case None =>
        for k <- completePatterns.keySet do
          nodes.remove(k)

        None

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    nodes.filterInPlace: (messageIdxs, _) =>
      messageIdxsToRemove.forall(i => !messageIdxs.contains(i))
