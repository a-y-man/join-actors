package join_patterns

import actor.ActorRef
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.TreeMap

import math.Ordering.Implicits.infixOrderingOps
import math.Ordering.Implicits.seqOrdering

type MessageIdxs = List[Int]

type Messages[M] = List[M]

type LookupEnv = Map[String, Any]
object LookupEnv:
  def apply[String, Any](elems: (String, Any)*) = Map[String, Any](elems*)

  def empty: LookupEnv = Map.empty[String, Any]

type PatternFits[M] = Set[((M => Boolean, M => LookupEnv), Int)]
object PatternFits:
  def apply[M](): PatternFits[M] = Set.empty

type PatternState[M, T] = ((JoinPattern[M, T], Int), MatchingTree[M])

// Define a custom ordering for lists based on size and value equality
given listOrdering[T: Ordering]: Ordering[List[T]] with
  def compare(x: List[T], y: List[T]): Int =
    val sizeComp = x.size.compareTo(y.size) // compare by size first
    if sizeComp != 0 then -sizeComp // if sizes are different, return the comparison result
    else
      x.zip(y).foldLeft(0) { // otherwise, compare each element pair
        case (acc, (a, b)) if acc != 0 => acc // if already found a difference, return it
        case (_, (a, b)) => Ordering[T].compare(a, b) // else, compare the elements
      }

type NodeMapping[M] = TreeMap[MessageIdxs, PatternFits[M]]
object NodeMapping:
  def apply[M](): TreeMap[MessageIdxs, PatternFits[M]] =
    TreeMap[MessageIdxs, PatternFits[M]](List.empty -> Set.empty)

case class MatchingTree[M](
    val nodeMapping: NodeMapping[M] = NodeMapping()
) extends LazyLogging:
  def isEmpty: Boolean =
    nodeMapping.isEmpty

  def pruneTree(idxsToRemove: MessageIdxs): MatchingTree[M] =
    val updatedNodeMapping =
      nodeMapping.view.filterKeys(node => node.forall(i => !idxsToRemove.contains(i)))
    // logger.debug(s"Removed nodes containing indices: $idxsToRemove")
    MatchingTree(TreeMap.from(updatedNodeMapping))

  def removeNode(node: MessageIdxs): MatchingTree[M] =
    MatchingTree(nodeMapping - node)

  def ppTree: String =
    nodeMapping
      .map { (nodes, candidates) =>
        val nodesToStr = s"${nodes.mkString("{ ", ", ", " }")}"
        val candidatesToStr =
          candidates
            .map(x => s"(${x._2}, MSG-CLOSURE, FIELDS-CLOSURE)")
            .mkString("{ ", ", ", " }")
        s"${nodesToStr}\t -> ${candidatesToStr}"
      }
      .mkString("\n")

  def logMapping(ident: String): Unit =
    logger.debug(
      s"\n\n************$ident****************\n${ppTree}\n***************$ident*************\n"
    )

/** An ADT defintion of a join pattern
  */
case class JoinPattern[M, T](
    extract: Messages[M] => Option[(Iterator[MessageIdxs], PatternFits[M])],
    guard: LookupEnv => Boolean,
    rhs: (LookupEnv, ActorRef[M]) => T,
    size: Int,
    partialExtract: (Tuple2[M, Int], MatchingTree[M]) => Option[MatchingTree[M]]
)

enum MatchingAlgorithm:
  case BruteForceAlgorithm, StatefulTreeBasedAlgorithm
