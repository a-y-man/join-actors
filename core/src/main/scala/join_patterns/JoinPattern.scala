package join_patterns.types

import join_actors.actor.ActorRef

import scala.collection.Factory
import scala.collection.immutable.{ArraySeq, TreeMap}
import scala.collection.mutable.Builder

type MessageIdx  = Int

type MessageIdxs = ArraySeq[MessageIdx]
object MessageIdxs extends Factory[MessageIdx, MessageIdxs]:
  def apply(elems: MessageIdx*): MessageIdxs = ArraySeq(elems*)

  def fromSpecific(it: IterableOnce[MessageIdx]): MessageIdxs =
    it.iterator.to(ArraySeq)

  def newBuilder: Builder[MessageIdx, MessageIdxs] =
    ArraySeq.newBuilder[MessageIdx]


type PatternIdx  = Int

type PatternIdxs = ArraySeq[PatternIdx]
object PatternIdxs extends Factory[PatternIdx, PatternIdxs]:
  def apply(elems: PatternIdx*): PatternIdxs = ArraySeq(elems*)

  def fromSpecific(it: IterableOnce[PatternIdx]): PatternIdxs =
    it.iterator.to(ArraySeq)

  def newBuilder: Builder[PatternIdx, PatternIdxs] =
    ArraySeq.newBuilder[PatternIdx]


given sizeBiasedOrdering: Ordering[ArraySeq[PatternIdx]] with
  def compare(x: ArraySeq[PatternIdx], y: ArraySeq[PatternIdx]): Int =
    val sizeComp = Integer.compare(x.length, y.length) // compare by size first
    if sizeComp != 0 then -sizeComp // if sizes are different, return the comparison result
    else
      var acc = 0
      var i   = 0
      while i < x.size && i < y.size && acc == 0 do
        val a = x(i)
        val b = y(i)
        if a != b then acc = Integer.compare(a, b)
        i += 1
      acc

/**
 * A map from constructor indices within a pattern to the indices of the messages that match the pattern.
 * If a constructor appears once, then the map key is a list with one index. If the same constructor appears multiple
 * times, the key is a list of multiple indices.
 */
type PatternBins = TreeMap[PatternIdxs, MessageIdxs]
object PatternBins:
  def apply(elems: (PatternIdxs, MessageIdxs)*): PatternBins =
    TreeMap[PatternIdxs, MessageIdxs](elems*)

def ppPatternBins(patternBins: PatternBins): String =
  patternBins
    .map { case (patternShape, messageIdxs) =>
      val patternShapeStr = patternShape.mkString(", ")
      val messageIdxsStr  = messageIdxs.mkString(", ")
      s"{ ${Console.RED + patternShapeStr + Console.RESET} } -> [ ${Console.GREEN + messageIdxsStr + Console.RESET} ]"
    }
    .mkString(", ")

final case class PatternIdxInfo[M](
  msgTypeChecker: M => Boolean,
  lookupEnvExtractor: M => LookupEnv,
  filterer: LookupEnv => Boolean
)

type PatternExtractors[M] = Map[PatternIdx, PatternIdxInfo[M]]
object PatternExtractors:
  def apply[M](elems: (PatternIdx, PatternIdxInfo[M])*): PatternExtractors[M] =
    Map[PatternIdx, PatternIdxInfo[M]](elems*)

def ppPatternExtractors[M, T](patternExtractors: PatternExtractors[M]): String =
  patternExtractors
    .map { case (patternIdx, PatternIdxInfo(checkMsgType, fieldExtractor, _)) =>
      val patternIdxStr = Console.YELLOW + patternIdx.toString + Console.RESET
      s"${patternIdxStr} -> { ${Console.BLUE}M => Boolean${Console.RESET}, ${Console.BLUE}M => LookupEnv${Console.RESET} }"
    }
    .mkString(", ")

final case class PatternInfo[M](
    // Initial pattern bins
    patternBins: PatternBins,
    patternExtractors: PatternExtractors[M]
)

type Messages[M] = Map[Int, M]

type LookupEnv = Map[String, Any]
object LookupEnv:
  def apply(elems: (String, Any)*): LookupEnv = Map[String, Any](elems*)

  def empty: LookupEnv = Map.empty[String, Any]

def ppLookupEnv(lookupEnv: LookupEnv): String =
  lookupEnv
    .map { case (k, v) => s"\u001b[32m${k}\u001b[0m \u2192 \u001b[34m${v}\u001b[0m" }
    .mkString("{ ", ", ", " }")

/** An ADT definition of a join pattern
  */
case class JoinPattern[M, T](
    guard: LookupEnv => Boolean,
    rhs: (LookupEnv, ActorRef[M]) => T,
    size: Int,
    getPatternInfo: PatternInfo[M]
)

