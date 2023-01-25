package join_patterns

//                 Msgs from Q     | Pattern Idxs from Pattern case
//                 [ Ø            -> {} ]
//                 [ {1}          -> { [1], [3] }]
//                 [ {1, 2}       -> { [1, 2], [3, 2] }]
//                 [ {2}          -> { [2] }]
type NodeMapping = Map[Set[Int], Set[List[Int]]]
object NodeMapping:
  def apply() : Map[Set[Int], Set[List[Int]]] =
    Map[Set[Int], Set[List[Int]]]()

// Edges
//               { (Ø, {1}), ({1}, {1, 3}), ({1, 2},  ) }
type TreeEdges = Set[Tuple2[Set[Int], Set[Int]]]
object TreeEdges:
  def apply() : Set[Tuple2[Set[Int], Set[Int]]] =
    Set[Tuple2[Set[Int], Set[Int]]]()


case class MatchingTree(
  nodeMapping : NodeMapping,
  treeEdges : TreeEdges
) {
  def isEmpty : Boolean =
    nodeMapping.isEmpty && treeEdges.isEmpty
}



/** An ADT defintion of a join pattern
  *
  * */
case class JoinPattern[M, T](
    extract: List[M] => (Option[List[M]], List[(Int, M)], Map[String, Any]),
    guard: Map[String, Any] => Boolean,
    rhs: Map[String, Any] => T,
    size: Int,
    partialExtract : (List[M], MatchingTree) => Option[MatchingTree]
)