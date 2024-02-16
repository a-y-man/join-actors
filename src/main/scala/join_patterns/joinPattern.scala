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

type NodeMapping[M] = TreeMap[MessageIdxs, PatternFits[M]]
object NodeMapping:
  def apply[M](): TreeMap[MessageIdxs, PatternFits[M]] =
    TreeMap[MessageIdxs, PatternFits[M]](List.empty -> Set.empty)(
      Ordering.by[MessageIdxs, Int](-_.size)
    )

case class MatchingTree[M](
    val nodeMapping: NodeMapping[M] = NodeMapping()
) extends LazyLogging:
  def isEmpty: Boolean =
    nodeMapping.isEmpty

  def pruneTree(idxsToRemove: MessageIdxs): MatchingTree[M] =
    val updatedNodeMapping =
      nodeMapping.view.filterKeys(node => node.forall(i => !idxsToRemove.contains(i)))
    // logger.info(s"Removed nodes containing indices: $idxsToRemove")
    MatchingTree(TreeMap.from(updatedNodeMapping))

  def removeNode(node: MessageIdxs): MatchingTree[M] =
    MatchingTree(nodeMapping - node)

  def ppTree: String =
    nodeMapping
      .map { (nodes, candidates) =>
        val nodesToStr = s"${nodes.mkString("{ ", ", ", " }")}"
        val candidatesToStr =
          candidates
            .map(x => s"${x._2}") // ", MSG-CLOSURE, FIELDS-CLOSURE)")
            .mkString("{ ", ", ", " }")
        s"${nodesToStr}\t -> ${candidatesToStr}"
      }
      .mkString("\n")

  def logMapping(ident: String, patIdx: Int): Unit =
    logger.info(
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
