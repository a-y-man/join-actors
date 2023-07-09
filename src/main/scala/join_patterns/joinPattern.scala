package join_patterns

import scala.collection.immutable.TreeMap

import math.Ordering.Implicits.{infixOrderingOps, seqOrdering}

type NodeMapping[M] = TreeMap[List[Int], Set[(Int, M => Boolean, M => Map[String, Any])]]
object NodeMapping:
  def apply[M](): TreeMap[List[Int], Set[(Int, M => Boolean, M => Map[String, Any])]] =
    TreeMap[List[Int], Set[(Int, M => Boolean, M => Map[String, Any])]](List.empty -> Set.empty)(
      Ordering.by[List[Int], Int](-_.size)
    )

case class MatchingTree[M](
    val nodeMapping: NodeMapping[M] = NodeMapping()
) {
  def isEmpty: Boolean =
    nodeMapping.isEmpty

  def pruneTree(idxsToRemove: List[Int]): MatchingTree[M] =
    val updatedNodeMapping =
      nodeMapping.view.filterKeys(node => node.forall(i => !idxsToRemove.contains(i)))

    MatchingTree(TreeMap.from(updatedNodeMapping))

  def removeNode(node: List[Int]): MatchingTree[M] =
    MatchingTree(nodeMapping - node)
}

def printMapping[M](mapping: NodeMapping[M]): Unit =
  mapping.foreach { (nodes, candidates) =>
    val nodesToStr = s"${nodes.mkString("{ ", ", ", " }")}"
    val candidatesToStr =
      candidates.map(x => s"(${x._1}, MSG-CLOSURE, FIELDS-CLOSURE)").mkString("{ ", ", ", " }")
    val mToStr = s"[ ${nodesToStr}\t -> ${candidatesToStr} ]"
    println(mToStr)
  }

/** An ADT defintion of a join pattern
  */
case class JoinPattern[M, T](
    extract: List[M] => Option[(List[Int], Set[(Int, M => Boolean, M => Map[String, Any])])],
    guard: Map[String, Any] => Boolean,
    rhs: Map[String, Any] => T,
    size: Int,
    partialExtract: (List[M], MatchingTree[M]) => Option[MatchingTree[M]]
)

enum MatchingAlgorithm:
  case BasicAlgorithm, TreeBasedAlgorithm
