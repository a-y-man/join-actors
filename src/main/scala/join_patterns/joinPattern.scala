package join_patterns

import actor.ActorRef
import com.typesafe.scalalogging.LazyLogging

import math.Ordering.Implicits.infixOrderingOps
import math.Ordering.Implicits.seqOrdering

type Messages[M] = List[M]

type LookupEnv = Map[String, Any]
object LookupEnv:
  def apply[String, Any](elems: (String, Any)*) = Map[String, Any](elems*)

  def empty: LookupEnv = Map.empty[String, Any]

def ppLookupEnv(lookupEnv: LookupEnv): String =
  lookupEnv
    .map { case (k, v) => s"\u001b[32m${k}\u001b[0m \u2192 \u001b[34m${v}\u001b[0m" }
    .mkString("{ ", ", ", " }")

type PatternFits[M] = Set[((M => Boolean, M => LookupEnv), Int)]
object PatternFits:
  def apply[M](): PatternFits[M] = Set.empty

/** An ADT defintion of a join pattern
  */
case class JoinPattern[M, T](
    extract: Messages[M] => Option[(Iterator[MessageIdxs], PatternFits[M])],
    guard: LookupEnv => Boolean,
    rhs: (LookupEnv, ActorRef[M]) => T,
    size: Int,
    partialExtract: (
        Tuple2[M, Int],
        (MatchingTree, PatternExtractors[M, T])
    ) => Option[(MatchingTree, PatternExtractors[M, T])]
)

enum MatchingAlgorithm:
  case BruteForceAlgorithm, StatefulTreeBasedAlgorithm
