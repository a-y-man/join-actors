package join_patterns

import scala.Console
import scala.collection.immutable.*

import math.Ordering.Implicits.infixOrderingOps
import math.Ordering.Implicits.seqOrdering

type MessageIdx  = Int
type MessageIdxs = Queue[MessageIdx]
object MessageIdxs:
  def apply(elems: MessageIdx*): MessageIdxs = Queue(elems*)

type PatternIdx  = Int
type PatternIdxs = List[PatternIdx]
object PatternIdxs:
  def apply(elems: PatternIdx*): PatternIdxs = List(elems*)

type PatternBins = Map[PatternIdxs, MessageIdxs]
object PatternBins:
  def apply(elems: (PatternIdxs, MessageIdxs)*) = Map[PatternIdxs, MessageIdxs](elems*)

def ppPatternBins(patternBins: PatternBins): String =
  patternBins
    .map { case (patternShape, messageIdxs) =>
      val patternShapeStr = patternShape.mkString(", ")
      val messageIdxsStr  = messageIdxs.mkString(", ")
      s"{ ${Console.RED + patternShapeStr + Console.RESET} } -> [ ${Console.GREEN + messageIdxsStr + Console.RESET} ]"
    }
    .mkString(", ")

type PatternExtractors[M, T] = Map[PatternIdx, (M => Boolean, M => LookupEnv)]
object PatternExtractors:
  def apply[M, T](elems: (PatternIdx, (M => Boolean, M => LookupEnv))*): PatternExtractors[M, T] =
    Map[PatternIdx, (M => Boolean, M => LookupEnv)](elems*)

def ppPatternExtractors[M, T](patternExtractors: PatternExtractors[M, T]): String =
  patternExtractors
    .map { case (patternIdx, (checkMsgType, fieldExtractor)) =>
      val patternIdxStr = Console.YELLOW + patternIdx.toString + Console.RESET
      s"${patternIdxStr} -> { ${Console.BLUE}M => Boolean${Console.RESET}, ${Console.BLUE}M => LookupEnv${Console.RESET} }"
    }
    .mkString(", ")

final case class PatternInfo[M, T](
    val patternBins: PatternBins,
    val patternExtractors: PatternExtractors[M, T]
)

type PatternState[M, T] = ((JoinPattern[M, T], PatternIdx), (MatchingTree, PatternInfo[M, T]))

given messageIdxOrdering: Ordering[MessageIdxs] with
  def compare(x: MessageIdxs, y: MessageIdxs): Int =
    val sizeComp = x.size.compareTo(y.size) // compare by size first
    if sizeComp != 0 then -sizeComp // if sizes are different, return the comparison result
    else
      x.lazyZip(y).foldLeft(0) { // otherwise, compare each element pair
        case (acc, (a, b)) if acc != 0 => acc // if already found a difference, return it
        case (_, (a, b)) => Ordering[Int].compare(a, b) // else, compare the elements
      }

type MatchingTree = TreeMap[MessageIdxs, PatternBins]
object MatchingTree:
  def apply(elems: (MessageIdxs, PatternBins)*): TreeMap[MessageIdxs, PatternBins] =
    TreeMap[MessageIdxs, PatternBins](elems*)

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

// M = [ 3, 4, 5 ]  P = [ 0, 1, 2 ]

// 3 -> { 0, 2 }
// 4 -> { 1 }
// 5 -> { 0, 2 }

// { 3, 4, 5 } -> [{ 0, 2 } -> { 3, 5 }, { 1 } -> { 4 }]]
// { 3, 4 }    -> [{ 0, 2 } -> { 3 },    { 1 } -> { 4 }]
// { 3, 5 }    -> [{ 0, 2 } -> { 3, 5 }, { 1 } -> {   }]
// { 4, 5 }    -> [{ 0, 2 } -> { 5 },    { 1 } -> { 4 }]
// { 3 }       -> [{ 0, 2 } -> { 3 },    { 1 } -> {   }]
// { 4 }       -> [{ 0, 2 } -> {   },    { 1 } -> { 4  }]
// { 5 }       -> [{ 0, 2 } -> { 5 },    { 1 } -> {   }]
// [{ }        -> [{ 0, 2 } -> {   },    { 1 } -> {   }]

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

def logMapping(ppTree: String, ident: String): Unit =
  logger.info(
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

def msgIdxsToFits(patternBins: PatternBins): Map[MessageIdx, PatternIdxs] =
  // patternBins: [[3, 5] -> [0, 2], [4] -> [1]]
  // msgIdxsToFits: [3 -> [0, 2], 4 -> [1], 5 -> [0, 2]]
  patternBins.flatMap { case (patternShape, messageIdxs) =>
    messageIdxs.map { messageIdx =>
      (messageIdx, patternShape)
    }
  }

def findValidPermutations[M, T](
    msgIdxs: MessageIdxs,
    patExtractors: PatternExtractors[M, T],
    patternBins: PatternBins
): Iterator[Seq[(Int, M => LookupEnv)]] =

  // [3 -> [0, 2], 4 -> [1], 5 -> [0, 2]]
  val msgToPatternIndices = msgIdxsToFits(patternBins)

  def isValidPermutation(permutation: MessageIdxs): Boolean =
    permutation.zipWithIndex.forall { (msgIdx, permIdx) =>
      // P  [0, 1, 2]
      // M  [3, 4, 5] || [5, 4, 3]
      msgToPatternIndices(msgIdx).contains(permIdx)
    }

  val validPermutations =
    msgIdxs.permutations.collect {
      case permutation if isValidPermutation(permutation) =>
        permutation.map { msgIdx =>
          val validPatternIdxs  = msgToPatternIndices(msgIdx)
          val i                 = permutation.indexOf(msgIdx)
          val (_, extractField) = patExtractors(validPatternIdxs.find(_ == i).get)
          (msgIdx, extractField)
        }
    }
  validPermutations

def processMessages(
    mtree: MatchingTree,
    messageIdxWithFits: List[(MessageIdx, PatternIdxs)]
): MatchingTree =
  messageIdxWithFits.foldLeft(mtree) { case (accMTree, (messageIdx, patternShape)) =>
    updateMTree(accMTree, messageIdx, patternShape)
  }
