package test

import actor.*
import join_patterns.*
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.LinkedTransferQueue
import scala.collection.immutable.List
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Random

val exampleTree = MatchingTree(
  List(0, 1, 2) -> PatternBins(
    List(0) -> List(0),
    List(1) -> List(1),
    List(2) -> List(2)
  ),
  List(0, 1) -> PatternBins(
    List(0) -> List(0),
    List(1) -> List(1),
    List(2) -> List()
  ),
  List(0, 2) -> PatternBins(
    List(0) -> List(0),
    List(1) -> List(),
    List(2) -> List(2)
  ),
  List(1, 2) -> PatternBins(
    List(0) -> List(),
    List(1) -> List(1),
    List(2) -> List(2)
  ),
  List(0) -> PatternBins(
    List(0) -> List(0),
    List(1) -> List(),
    List(2) -> List()
  ),
  List(1) -> PatternBins(
    List(0) -> List(),
    List(1) -> List(1),
    List(2) -> List()
  ),
  List(2) -> PatternBins(
    List(0) -> List(),
    List(1) -> List(),
    List(2) -> List(2)
  ),
  List() -> PatternBins(
    List(0) -> List(),
    List(1) -> List(),
    List(2) -> List()
  )
)

class MatchingTreeTests extends AnyFunSuite:
  test("Disjoint types in pattern -- Single message tree update") {
    val bins = PatternBins(
      List(0) -> List(),
      List(1) -> List(),
      List(2) -> List()
    )

    val tree = MatchingTree(List.empty[Int] -> bins)

    val patIdxs                             = List(0, 1, 2)
    val newMsg @ (newMsgIdx, newMsgMatches) = (0, List(0))

    val newTree = updateMTree(tree, newMsgIdx, newMsgMatches)

    assert(
      newTree == tree.updated(
        List(newMsgIdx),
        PatternBins(
          List(0) -> List(newMsgIdx),
          List(1) -> List(),
          List(2) -> List()
        )
      )
    )
  }

  test("Disjoint types in pattern -- Multiple message tree update") {
    val bins = PatternBins(
      List(0) -> List(),
      List(1) -> List(),
      List(2) -> List()
    )

    val tree = MatchingTree(List.empty[Int] -> bins)

    val patIdxs                                = List(0, 1, 2)
    val newMsg0 @ (newMsgIdx0, newMsgMatches1) = (0, List(0))
    val newMsg1 @ (newMsgIdx1, newMsgMatches2) = (1, List(1))
    val newMsg2 @ (newMsgIdx2, newMsgMatches3) = (2, List(2))

    val newTree0 = updateMTree(tree, newMsgIdx0, newMsgMatches1)
    val newTree1 = updateMTree(newTree0, newMsgIdx1, newMsgMatches2)
    val newTree2 = updateMTree(newTree1, newMsgIdx2, newMsgMatches3)

    assert {
      newTree0 ==
        tree ++ MatchingTree(
          List(newMsgIdx0) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(),
            List(2) -> List()
          )
        )
    }

    assert {
      newTree1 == tree ++
        MatchingTree(
          List(newMsgIdx0, newMsgIdx1) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(newMsgIdx1),
            List(2) -> List()
          ),
          List(newMsgIdx0) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(),
            List(2) -> List()
          ),
          List(newMsgIdx1) -> PatternBins(
            List(0) -> List(),
            List(1) -> List(newMsgIdx1),
            List(2) -> List()
          )
        )
    }

    assert {
      newTree2 == tree ++
        MatchingTree(
          List(newMsgIdx0, newMsgIdx1, newMsgIdx2) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(newMsgIdx1),
            List(2) -> List(newMsgIdx2)
          ),
          List(newMsgIdx0, newMsgIdx1) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(newMsgIdx1),
            List(2) -> List()
          ),
          List(newMsgIdx0, newMsgIdx2) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(),
            List(2) -> List(newMsgIdx2)
          ),
          List(newMsgIdx1, newMsgIdx2) -> PatternBins(
            List(0) -> List(),
            List(1) -> List(newMsgIdx1),
            List(2) -> List(newMsgIdx2)
          ),
          List(newMsgIdx0) -> PatternBins(
            List(0) -> List(newMsgIdx0),
            List(1) -> List(),
            List(2) -> List()
          ),
          List(newMsgIdx1) -> PatternBins(
            List(0) -> List(),
            List(1) -> List(newMsgIdx1),
            List(2) -> List()
          ),
          List(newMsgIdx2) -> PatternBins(
            List(0) -> List(),
            List(1) -> List(),
            List(2) -> List(newMsgIdx2)
          )
        )
    }
  }

  test("Duplicate Types in pattern -- Single message tree update") {
    val bins = PatternBins(
      List(0, 2) -> List(),
      List(1)    -> List()
    )

    val tree = MatchingTree(List.empty[Int] -> bins)

    val patIdxs                             = List(0, 1, 2)
    val newMsg @ (newMsgIdx, newMsgMatches) = (0, List(0, 2))

    val newTree = updateMTree(tree, newMsgIdx, newMsgMatches)

    assert {
      newTree == tree ++ MatchingTree(
        List(newMsgIdx) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx),
            List(1)    -> List()
          )
      )
    }
  }

  test("Duplicate Types in pattern -- Multiple message tree update") {
    val bins = PatternBins(
      List(0, 2) -> List(),
      List(1)    -> List()
    )

    val tree = MatchingTree(List.empty[Int] -> bins)

    val patIdxs                                = List(0, 1, 2)
    val newMsg0 @ (newMsgIdx0, newMsgMatches1) = (0, List(0, 2))
    val newMsg1 @ (newMsgIdx1, newMsgMatches2) = (1, List(1))
    val newMsg2 @ (newMsgIdx2, newMsgMatches3) = (2, List(0, 2))

    val newTree0 = updateMTree(tree, newMsgIdx0, newMsgMatches1)
    val newTree1 = updateMTree(newTree0, newMsgIdx1, newMsgMatches2)
    val newTree2 = updateMTree(newTree1, newMsgIdx2, newMsgMatches3)

    assert {
      newTree0 == tree ++ MatchingTree(
        List(newMsgIdx0) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0),
            List(1)    -> List()
          )
      )
    }

    assert {
      newTree1 == tree ++ MatchingTree(
        List(newMsgIdx0, newMsgIdx1) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0),
            List(1)    -> List(newMsgIdx1)
          ),
        List(newMsgIdx0) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0),
            List(1)    -> List()
          ),
        List(newMsgIdx1) ->
          PatternBins(
            List(0, 2) -> List(),
            List(1)    -> List(newMsgIdx1)
          )
      )
    }

    assert {
      newTree2 == tree ++ MatchingTree(
        List(newMsgIdx0, newMsgIdx1, newMsgIdx2) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0, newMsgIdx2),
            List(1)    -> List(newMsgIdx1)
          ),
        List(newMsgIdx0, newMsgIdx1) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0),
            List(1)    -> List(newMsgIdx1)
          ),
        List(newMsgIdx0, newMsgIdx2) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0, newMsgIdx2),
            List(1)    -> List()
          ),
        List(newMsgIdx1, newMsgIdx2) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx2),
            List(1)    -> List(newMsgIdx1)
          ),
        List(newMsgIdx0) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx0),
            List(1)    -> List()
          ),
        List(newMsgIdx1) ->
          PatternBins(
            List(0, 2) -> List(),
            List(1)    -> List(newMsgIdx1)
          ),
        List(newMsgIdx2) ->
          PatternBins(
            List(0, 2) -> List(newMsgIdx2),
            List(1)    -> List()
          )
      )
    }
  }

  test("Find complete patterns in MatchingTree") {
    val mtree: MatchingTree = MatchingTree(
      List(1, 2)    -> PatternBins(List(1, 2) -> List(1, 2)),
      List(1, 2, 3) -> PatternBins(List(1, 2, 3) -> List(1, 2, 3))
    )

    val patternSize = 3

    val result = findCompletePatterns(mtree, patternSize)

    val expected: MatchingTree = MatchingTree(
      List(1, 2, 3) -> PatternBins(List(1, 2, 3) -> List(1, 2, 3))
    )

    assert(result == expected)
  }

  test("Find complete patterns in MatchingTree -- Empty MatchingTree") {
    val mtree: MatchingTree = MatchingTree()
    val patternSize         = 3
    val result              = findCompletePatterns(mtree, patternSize)
    assert(result.isEmpty)
  }

  test("Find complete patterns in MatchingTree -- No complete patterns of given size") {
    val mtree: MatchingTree = MatchingTree(
      List(1, 2)       -> PatternBins(List(1, 2) -> List(1, 2)),
      List(1, 2, 3, 4) -> PatternBins(List(1, 2, 3, 4) -> List(1, 2, 3, 4))
    )
    val patternSize = 3
    val result      = findCompletePatterns(mtree, patternSize)
    assert(result.isEmpty)
  }

  test("Find complete patterns in MatchingTree -- Multiple complete patterns of given size") {
    val mtree: MatchingTree = MatchingTree(
      List(1, 2, 3) -> PatternBins(List(1, 2, 3) -> List(1, 2, 3)),
      List(4, 5, 6) -> PatternBins(List(4, 5, 6) -> List(4, 5, 6))
    )
    val patternSize = 3
    val result      = findCompletePatterns(mtree, patternSize)
    assert(result == mtree)
  }

  test("removeNode should remove the specified node from the tree") {
    val mtree: MatchingTree = MatchingTree(
      List(1, 2) -> PatternBins(List(1, 2) -> List(1, 2)),
      List(3, 4) -> PatternBins(List(3, 4) -> List(3, 4))
    )

    val messageIdxsToRemove = List(1, 2)

    val result = removeNode(mtree, messageIdxsToRemove)
    assert(
      result == MatchingTree(
        List(3, 4) -> PatternBins(List(3, 4) -> List(3, 4))
      )
    )
  }

  test(
    "Pruning a tree should remove nodes containing specified message indices -- Remove all nodes"
  ) {

    val prunedTree = pruneTree(exampleTree, List(0, 1, 2))

    assert(prunedTree.size == 1)
    assert(!prunedTree.contains(List(0)))
    assert(!prunedTree.contains(List(1)))
    assert(!prunedTree.contains(List(2)))
    assert(!prunedTree.contains(List(0, 1)))
    assert(!prunedTree.contains(List(0, 2)))
    assert(!prunedTree.contains(List(1, 2)))
    assert(!prunedTree.contains(List(0, 1, 2)))
  }

  test(
    "Pruning a tree should remove nodes containing specified message indices -- Remove some nodes"
  ) {

    val prunedTree = pruneTree(exampleTree, List(0, 1))

    assert(prunedTree.size == 2)
    assert(prunedTree.contains(List()))
    assert(prunedTree.contains(List(2)))
    assert(!prunedTree.contains(List(0)))
    assert(!prunedTree.contains(List(1)))
    assert(!prunedTree.contains(List(0, 1)))
    assert(!prunedTree.contains(List(0, 2)))
    assert(!prunedTree.contains(List(1, 2)))
  }

  test("Remove node from tree given message indices") {
    val updatedTree = removeNode(exampleTree, List(0, 1, 2))

    assert(updatedTree.size == 7)
    assert(!updatedTree.contains(List(0, 1, 2)))
    assert(updatedTree.contains(List()))
    assert(updatedTree.contains(List(0)))
    assert(updatedTree.contains(List(1)))
    assert(updatedTree.contains(List(2)))
    assert(updatedTree.contains(List(0, 1)))
    assert(updatedTree.contains(List(0, 2)))
    assert(updatedTree.contains(List(1, 2)))
  }
