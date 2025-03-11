package join_patterns.mutable

import join_actors.actor.ActorRef
import join_patterns.matcher.{CandidateMatch, Matcher}
import join_patterns.matching_tree.MatchingTree
import join_patterns.types.{JoinPattern, LookupEnv, MessageIdxs, PatternBins, given}

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.immutable.{ArraySeq, TreeMap}
import scala.collection.mutable.{ArrayBuffer, Map as MutableMap, TreeMap as MutableTreeMap}

class MutableMatchingTree[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int) extends Matcher[M, T]:
  // TODO: Currenly extending Matcher just to get the utility methods, this needs a refactor

  private val patternExtractors = pattern.getPatternInfo.patternExtractors
//  private val msgTypeCheckers = patternExtractors.map{ case (key, (typeChecker, _)) => (key, typeChecker) }

  private val nodes = MutableTreeMap[MessageIdxs, PatternBins](MessageIdxs() -> pattern.getPatternInfo.patternBins)


  private def updateTree(newMessageIdx: Int, msg: M): MutableMap[MessageIdxs, PatternBins] =
//    println()
//    println(s"Tree $patternIdx before update")
//    for tup <- nodes do
//      println(s"\t$tup")

//    println(s"New message index: $newMessageIdx")

    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, (msgTypeChecker, _msgFieldExtractor)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(ArraySeq)

//    println(s"Matching constructor indices: $matchingConstructorIdxs")

    if matchingConstructorIdxs.isEmpty then MutableTreeMap()
    else
      val additions = ArrayBuffer[(MessageIdxs, PatternBins)]()
      val completePatterns = MutableTreeMap[MessageIdxs, PatternBins]()

      for (messageIdxsMatched, bins) <- nodes do
        // Create the child for one leaf in the matching tree

        // If the PatternBins contains a key for the constructor type of the new message, we might be able to add a child
        bins.get(matchingConstructorIdxs).foreach { mappedMessageIdxs =>
//          println(s"Found key for $matchingConstructorIdxs")

          // We only add a new node if some of the constructor instances in the pattern don't already have a match
          if mappedMessageIdxs.size < matchingConstructorIdxs.size then
//            println(s"Adding node for $matchingConstructorIdxs")
            val newMessageIdxs = messageIdxsMatched :+ newMessageIdx
            val newPatternBins = bins.updated(matchingConstructorIdxs, mappedMessageIdxs :+ newMessageIdx)
            val newNode = (newMessageIdxs, newPatternBins)
            additions.append(newNode)

            if newMessageIdxs.size == pattern.size
              && newPatternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
            then completePatterns.addOne(newNode)
        }

//      println(s"Additions: $additions")

      nodes.addAll(additions)
//      for (k, v) <- additions do
//        nodes.update(k, v)

//      println(s"Tree $patternIdx after update")
//      for tup <- nodes do
//        println(s"\t$tup")

      completePatterns

//  def findCompletePatterns(patternSize: Int): MatchingTree =
//    nodes.view
//      .filterKeys { messageIdxs =>
//        messageIdxs.size == patternSize
//      }
//      .filter { case (_, patternBins) =>
//        patternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
//      }
//      .to(TreeMap)

  def findMatch(index: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatch[M, T] =
//    val updated = updateTree(index, msg)
//
//    if !updated then return None

//    val completePatterns = findCompletePatterns(pattern.size)

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
//        val toRemove = completePatterns.removed(bestMatchIdxs).keySet
        nodes.subtractAll(completePatterns.keySet)
//        for k <- completePatterns.removed(bestMatchIdxs).keySet do
//          nodes.remove(k)

        Some((bestMatchIdxs, patternIdx), selectedMatch)
      case None =>
        for k <- completePatterns.keySet do
          nodes.remove(k)

        None

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    nodes.filterInPlace: (messageIdxs, _) =>
      messageIdxsToRemove.forall(i => !messageIdxs.contains(i))

  override def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    // Needed because of extends Matcher
    ???