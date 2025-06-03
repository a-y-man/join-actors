package join_patterns.matching.array_while

import join_actors.actor.ActorRef
import join_patterns.matching.CandidateMatchOpt
import join_patterns.matching.functions.*
import join_patterns.types.{*, given}
import join_patterns.util.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Map as MutableMap, TreeMap as MutableTreeMap}
import scala.util.boundary
import scala.util.boundary.break

class WhileMatchingArray[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int):
  private val patternExtractors = pattern.getPatternInfo.patternExtractors
//  private val msgTypeCheckers = patternExtractors.map{ case (key, (typeChecker, _)) => (key, typeChecker) }

  type Node = (MessageIdxs, PatternBins)
  private var nodes = Array[Node](MessageIdxs() -> pattern.getPatternInfo.patternBins)


  private def updateTree(newMessageIdx: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =

    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, PatternIdxInfo(msgTypeChecker, _msgFieldExtractor, _)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(PatternIdxs)

    if matchingConstructorIdxs.isEmpty then None
    else
      val additions = new Array[Node](nodes.length)
      var additionIdx = 0

      val res = boundary:
        for (messageIdxsMatched, bins) <- nodes.fast do
          // Create the child for one leaf in the matching tree
          // If the PatternBins contains a key for the constructor type of the new message, we might be able to compute a child
          bins.get(matchingConstructorIdxs) match
            case None => ()
            case Some(mappedMessageIdxs) =>
              // We only add a new node if some of the constructor instances in the pattern don't already have a match
              if mappedMessageIdxs.size < matchingConstructorIdxs.size then
                val newMessageIdxs = messageIdxsMatched :+ newMessageIdx
                val newPatternBins = bins.updated(matchingConstructorIdxs, mappedMessageIdxs :+ newMessageIdx)

                if newMessageIdxs.size == pattern.size
                        && newPatternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
                then
                  // Find optimal permutation
                  val bestPermutation = findBestValidPermutation(newPatternBins, messages)

                  // If the guard can be satisfied, we break out of the loop with this permutation
                  // Otherwise, we do nothing. Either way, we do not add a new node to the tree
                  bestPermutation match
                    case r@Some(_) =>
                      break(r)
                    case None => ()
                else
                  val newNode = (newMessageIdxs, newPatternBins)
                  additions(additionIdx) = newNode
                  additionIdx += 1
        // If the loop does not find any full nodes with a valid permutation, we do not have a result
        None

      res match
        case Some((bestMatchIdxs, bestMatchSubsts)) =>
          val selectedMatch =
            (
                    bestMatchSubsts,
                    (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
            )

          Some((bestMatchIdxs, patternIdx), selectedMatch)
        case None =>
          // We only add nodes to the tree if no match was found
          nodes = sortedMerge(nodes, additions, additionIdx)
          None

  def findMatch(index: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =
    updateTree(index, msg, messages)

  private def findBestValidPermutation(patternBins: PatternBins, messages: MutableMap[Int, M]): Option[(MessageIdxs, LookupEnv)] =
    val validPermutations =
      getMsgIdxsWithPayloadExtractor(patternExtractors, patternBins)
    val bestMatchOpt = findFairestMatch(validPermutations, messages, pattern)
    bestMatchOpt

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    nodes = nodes.filter: (messageIdxs, _) =>
      messageIdxsToRemove.forall(i => !messageIdxs.contains(i))

  private def sortedMerge(arr1: Array[Node], arr2: Array[Node], numAdditions: Int) =
    val length1 = arr1.length
    val length2 = numAdditions
    val res = new Array[Node](length1 + length2)

    var i1 = 0
    var i2 = 0
    var ires = 0
    while i1 < length1 || i2 < length2 do
      if i1 < length1 && i2 < length2 then
        val v1 = arr1(i1)
        val v2 = arr2(i2)

        val comp = sizeBiasedOrdering.compare(v1._1, v2._1)

        if comp <= 0 then
          res(ires) = v1
          i1 += 1
        else
          res(ires) = v2
          i2 += 1
      else if i1 == length1 then
        res(ires) = arr2(i2)
        i2 += 1
      else
        res(ires) = arr1(i1)
        i1 += 1

      ires += 1

    res

