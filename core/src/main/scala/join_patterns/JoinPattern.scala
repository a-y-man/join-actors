package join_patterns.types

import join_patterns.matching_tree.*
import join_actors.actor.ActorRef

import math.Ordering.Implicits.infixOrderingOps
import math.Ordering.Implicits.seqOrdering
import scala.collection.immutable.Queue
import scala.collection.immutable.TreeMap

type MessageIdx  = Int
type MessageIdxs = Queue[MessageIdx]
object MessageIdxs:
  def apply(elems: MessageIdx*): MessageIdxs = Queue(elems*)

type PatternIdx  = Int
type PatternIdxs = List[PatternIdx]
object PatternIdxs:
  def apply(elems: PatternIdx*): PatternIdxs = List(elems*)

given patternIdxOrdering: Ordering[PatternIdxs] with
  def compare(x: PatternIdxs, y: PatternIdxs): Int =
    val sizeComp = x.size.compareTo(y.size) // compare by size first
    if sizeComp != 0 then -sizeComp // if sizes are different, return the comparison result
    else
      var acc = 0
      var i   = 0
      while i < x.size && i < y.size && acc == 0 do
        val a = x(i)
        val b = y(i)
        if a != b then acc = Ordering[Int].compare(a, b)
        i += 1
      acc

/**
 * A map from constructor indices within a pattern to the indices of the messages that match the pattern.
 * If a constructor appears once, then the map key is a list with one index. If the same constructor appears multiple
 * times, the key is a list of multiple indices.
 */
type PatternBins = TreeMap[PatternIdxs, MessageIdxs]
object PatternBins:
  def apply(elems: (PatternIdxs, MessageIdxs)*) =
    TreeMap[PatternIdxs, MessageIdxs](elems*)(patternIdxOrdering)

def ppPatternBins(patternBins: PatternBins): String =
  patternBins
    .map { case (patternShape, messageIdxs) =>
      val patternShapeStr = patternShape.mkString(", ")
      val messageIdxsStr  = messageIdxs.mkString(", ")
      s"{ ${Console.RED + patternShapeStr + Console.RESET} } -> [ ${Console.GREEN + messageIdxsStr + Console.RESET} ]"
    }
    .mkString(", ")

type PatternExtractors[M] = Map[PatternIdx, (M => Boolean, M => LookupEnv)]
object PatternExtractors:
  def apply[M](elems: (PatternIdx, (M => Boolean, M => LookupEnv))*): PatternExtractors[M] =
    Map[PatternIdx, (M => Boolean, M => LookupEnv)](elems*)

def ppPatternExtractors[M, T](patternExtractors: PatternExtractors[M]): String =
  patternExtractors
    .map { case (patternIdx, (checkMsgType, fieldExtractor)) =>
      val patternIdxStr = Console.YELLOW + patternIdx.toString + Console.RESET
      s"${patternIdxStr} -> { ${Console.BLUE}M => Boolean${Console.RESET}, ${Console.BLUE}M => LookupEnv${Console.RESET} }"
    }
    .mkString(", ")

final case class PatternInfo[M](
    // Initial pattern bins
    patternBins: PatternBins,
    patternExtractors: PatternExtractors[M]
)

/**
 * A tuple ((pattern, patternIdx), (mTree, patInfo))
 *
 * @tparam M The message type
 * @tparam T The result type
 */
type PatternState[M, T] = ((JoinPattern[M, T], Int), (MatchingTree, PatternInfo[M]))

given messageIdxOrdering: Ordering[MessageIdxs] with
  def compare(x: MessageIdxs, y: MessageIdxs): Int =
    val sizeComp = x.size.compareTo(y.size) // compare by size first
    if sizeComp != 0 then -sizeComp // if sizes are different, return the comparison result
    else
      var i   = 0
      var acc = 0
      while i < x.size && i < y.size && acc == 0 do
        val a = x(i)
        val b = y(i)
        if a != b then acc = Ordering[Int].compare(a, b)
        i += 1
      acc

type Messages[M] = Map[Int, M]

type LookupEnv = Map[String, Any]
object LookupEnv:
  def apply[String, Any](elems: (String, Any)*) = Map[String, Any](elems*)

  def empty: LookupEnv = Map.empty[String, Any]

def ppLookupEnv(lookupEnv: LookupEnv): String =
  lookupEnv
    .map { case (k, v) => s"\u001b[32m${k}\u001b[0m \u2192 \u001b[34m${v}\u001b[0m" }
    .mkString("{ ", ", ", " }")

/** An ADT defintion of a join pattern
  */
case class JoinPattern[M, T](
    extract: Messages[M] => Option[PatternBins],
    guard: LookupEnv => Boolean,
    rhs: (LookupEnv, ActorRef[M]) => T,
    size: Int,
    updateMTree: (
        (M, Int),
        MatchingTree
    ) => Option[MatchingTree],
    getPatternInfo: PatternInfo[M]
)

enum MatchingAlgorithm:
  case BruteForceAlgorithm, StatefulTreeBasedAlgorithm, MutableStatefulAlgorithm
