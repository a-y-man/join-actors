package join_patterns.matching_tree

import join_patterns.types.*
import join_patterns.types.given

import scala.Console
import scala.collection.immutable.*

import math.Ordering.Implicits.infixOrderingOps
import math.Ordering.Implicits.seqOrdering

/**
 * A map from the indices of the messages that have been matched so far to the pattern bins.
 */
type MatchingTree = TreeMap[MessageIdxs, PatternBins]
object MatchingTree:
  def apply(elems: (MessageIdxs, PatternBins)*): TreeMap[MessageIdxs, PatternBins] =
    TreeMap[MessageIdxs, PatternBins](elems*)(messageIdxOrdering)

def ppTree(mtree: MatchingTree): String =
  mtree
    .map { case (messageIdxs, patternBins) =>
      val messageIdxsStr =
        messageIdxs.map(idx => Console.GREEN + s"${idx}" + Console.RESET).mkString(", ")
      val patternBinsStr = ppPatternBins(patternBins)
      s"[ ${messageIdxsStr} ] -> { ${patternBinsStr} }"
    }
    .mkString("\n") +
    "\nThe tree has " + mtree.size + " nodes"

def updateMTree(
    mtree: MatchingTree,
    messageIdx: MessageIdx,
    patternShape: PatternIdxs
): MatchingTree =
  mtree.foldLeft(mtree) { case (acc, (messageIdxs, patternBins)) =>
    val updatedPatternBinsOpt = patternBins.get(patternShape) match
      case Some(messageIdxList) =>
        if messageIdxList.size == patternShape.size then None
        else Some(patternBins.updated(patternShape, messageIdxList :+ messageIdx))
      case None => None

    updatedPatternBinsOpt match
      case None                     => acc
      case Some(updatedPatternBins) => acc.updated(messageIdxs :+ messageIdx, updatedPatternBins)
  }

def pruneTree(mtree: MatchingTree, messageIdxsToRemove: MessageIdxs): MatchingTree =
  mtree.view
    .filterKeys { case messageIdxs =>
      messageIdxsToRemove.forall(i => !messageIdxs.contains(i))
    }
    .to(TreeMap)

def removeNode(mtree: MatchingTree, messageIdxsToRemove: MessageIdxs): MatchingTree =
  mtree - messageIdxsToRemove

def logMTree(ppTree: String, ident: String): Unit =
  println(
    s"\n\n************$ident****************\n${ppTree}\n***************$ident*************\n"
  )

def findCompletePatterns(mtree: MatchingTree, patternSize: Int): MatchingTree =
  mtree.view
    .filterKeys { case messageIdxs =>
      messageIdxs.size == patternSize
    }
    .filter { case (_, patternBins) =>
      patternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
    }
    .to(TreeMap)
