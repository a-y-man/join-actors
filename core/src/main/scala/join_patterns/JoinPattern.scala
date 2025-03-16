package join_patterns.types

import join_actors.actor.ActorRef
import join_patterns.matching.immutable.MatchingTree

import scala.annotation.targetName
import scala.collection.Factory
import scala.collection.immutable.{ArraySeq, TreeMap}
import scala.collection.mutable.Builder

type MessageIdx  = Int

final case class MessageIdxs private (delegate: ArraySeq[MessageIdx]) extends Iterable[MessageIdx]:
  export delegate.{:+ as _, apply, iterator, sorted, contains, combinations}

  @targetName("colonPlus")
  infix def :+(e: MessageIdx): MessageIdxs =
    MessageIdxs(delegate :+ e)

object MessageIdxs:
  def apply(elems: MessageIdx*): MessageIdxs = MessageIdxs(ArraySeq(elems*))

given messageIdxOrdering: Ordering[MessageIdxs] with
  def compare(x: MessageIdxs, y: MessageIdxs): Int =
    val sizeComparison = Ordering[Int].compare(x.size, y.size)

    val xs = x.sorted
    val ys = y.sorted

    if sizeComparison != 0 then
      val slexRes = slex(xs, ys)
      if slexRes == 0 then -sizeComparison else slexRes
    else slex(xs, ys)

  private def slex(xs: Seq[Int], ys: Seq[Int]): Int =
    val minLength = math.min(xs.size, ys.size)
    var i         = 0
    while i < minLength do
      val elementComparison = Ordering[Int].compare(xs(i), ys(i))
      if elementComparison != 0 then return elementComparison
      i += 1
    0

type PatternIdx  = Int
final case class PatternIdxs private (delegate: ArraySeq[PatternIdx])
  extends Iterable[PatternIdx]:
  export delegate.{apply, iterator}

object PatternIdxs extends Factory[PatternIdx, PatternIdxs]:
  def apply(elems: PatternIdx*): PatternIdxs = PatternIdxs(ArraySeq(elems*))

  def fromSpecific(it: IterableOnce[PatternIdx]): PatternIdxs =
    PatternIdxs(it.iterator.to(ArraySeq))

  def newBuilder: Builder[PatternIdx, PatternIdxs] =
    ArraySeq.newBuilder[PatternIdx].mapResult(PatternIdxs.apply)

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
  case BruteForceAlgorithm, StatefulTreeBasedAlgorithm, MutableStatefulAlgorithm, LazyMutableAlgorithm, WhileLazyAlgorithm
