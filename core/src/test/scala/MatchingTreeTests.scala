package test

import actor.*
import actor.Result.*
import join_patterns.*
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.LinkedTransferQueue
import scala.collection.immutable.List
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Random

val exampleTree = MatchingTree(
  MessageIdxs(0, 1, 2) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(0),
    PatternIdxs(1) -> MessageIdxs(1),
    PatternIdxs(2) -> MessageIdxs(2)
  ),
  MessageIdxs(0, 1) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(0),
    PatternIdxs(1) -> MessageIdxs(1),
    PatternIdxs(2) -> MessageIdxs()
  ),
  MessageIdxs(0, 2) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(0),
    PatternIdxs(1) -> MessageIdxs(),
    PatternIdxs(2) -> MessageIdxs(2)
  ),
  MessageIdxs(1, 2) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(),
    PatternIdxs(1) -> MessageIdxs(1),
    PatternIdxs(2) -> MessageIdxs(2)
  ),
  MessageIdxs(0) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(0),
    PatternIdxs(1) -> MessageIdxs(),
    PatternIdxs(2) -> MessageIdxs()
  ),
  MessageIdxs(1) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(),
    PatternIdxs(1) -> MessageIdxs(1),
    PatternIdxs(2) -> MessageIdxs()
  ),
  MessageIdxs(2) -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(),
    PatternIdxs(1) -> MessageIdxs(),
    PatternIdxs(2) -> MessageIdxs(2)
  ),
  MessageIdxs() -> PatternBins(
    PatternIdxs(0) -> MessageIdxs(),
    PatternIdxs(1) -> MessageIdxs(),
    PatternIdxs(2) -> MessageIdxs()
  )
)

class MatchingTreeTests extends AnyFunSuite:
  test("Disjoint types in pattern -- Single message tree update") {
    val bins = PatternBins(
      PatternIdxs(0) -> MessageIdxs(),
      PatternIdxs(1) -> MessageIdxs(),
      PatternIdxs(2) -> MessageIdxs()
    )

    val tree = MatchingTree(MessageIdxs() -> bins)

    val patIdxs                             = PatternIdxs(0, 1, 2)
    val newMsg @ (newMsgIdx, newMsgMatches) = (0, PatternIdxs(0))

    val newTree = updateMTree(tree, newMsgIdx, newMsgMatches)

    assert(
      newTree == tree.updated(
        MessageIdxs(newMsgIdx),
        PatternBins(
          PatternIdxs(0) -> MessageIdxs(newMsgIdx),
          PatternIdxs(1) -> MessageIdxs(),
          PatternIdxs(2) -> MessageIdxs()
        )
      )
    )
  }

  test("Disjoint types in pattern -- Multiple message tree update") {
    val bins = PatternBins(
      PatternIdxs(0) -> MessageIdxs(),
      PatternIdxs(1) -> MessageIdxs(),
      PatternIdxs(2) -> MessageIdxs()
    )

    val tree = MatchingTree(MessageIdxs() -> bins)

    val patIdxs                                = PatternIdxs(0, 1, 2)
    val newMsg0 @ (newMsgIdx0, newMsgMatches1) = (0, PatternIdxs(0))
    val newMsg1 @ (newMsgIdx1, newMsgMatches2) = (1, PatternIdxs(1))
    val newMsg2 @ (newMsgIdx2, newMsgMatches3) = (2, PatternIdxs(2))

    val newTree0 = updateMTree(tree, newMsgIdx0, newMsgMatches1)
    val newTree1 = updateMTree(newTree0, newMsgIdx1, newMsgMatches2)
    val newTree2 = updateMTree(newTree1, newMsgIdx2, newMsgMatches3)

    assert {
      newTree0 ==
        tree ++ MatchingTree(
          MessageIdxs(newMsgIdx0) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(),
            PatternIdxs(2) -> MessageIdxs()
          )
        )
    }

    assert {
      newTree1 == tree ++
        MatchingTree(
          MessageIdxs(newMsgIdx0, newMsgIdx1) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(newMsgIdx1),
            PatternIdxs(2) -> MessageIdxs()
          ),
          MessageIdxs(newMsgIdx0) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(),
            PatternIdxs(2) -> MessageIdxs()
          ),
          MessageIdxs(newMsgIdx1) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(),
            PatternIdxs(1) -> MessageIdxs(newMsgIdx1),
            PatternIdxs(2) -> MessageIdxs()
          )
        )
    }

    assert {
      newTree2 == tree ++
        MatchingTree(
          MessageIdxs(newMsgIdx0, newMsgIdx1, newMsgIdx2) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(newMsgIdx1),
            PatternIdxs(2) -> MessageIdxs(newMsgIdx2)
          ),
          MessageIdxs(newMsgIdx0, newMsgIdx1) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(newMsgIdx1),
            PatternIdxs(2) -> MessageIdxs()
          ),
          MessageIdxs(newMsgIdx0, newMsgIdx2) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(),
            PatternIdxs(2) -> MessageIdxs(newMsgIdx2)
          ),
          MessageIdxs(newMsgIdx1, newMsgIdx2) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(),
            PatternIdxs(1) -> MessageIdxs(newMsgIdx1),
            PatternIdxs(2) -> MessageIdxs(newMsgIdx2)
          ),
          MessageIdxs(newMsgIdx0) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1) -> MessageIdxs(),
            PatternIdxs(2) -> MessageIdxs()
          ),
          MessageIdxs(newMsgIdx1) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(),
            PatternIdxs(1) -> MessageIdxs(newMsgIdx1),
            PatternIdxs(2) -> MessageIdxs()
          ),
          MessageIdxs(newMsgIdx2) -> PatternBins(
            PatternIdxs(0) -> MessageIdxs(),
            PatternIdxs(1) -> MessageIdxs(),
            PatternIdxs(2) -> MessageIdxs(newMsgIdx2)
          )
        )
    }
  }

  test("Duplicate Types in pattern -- Single message tree update") {
    val bins = PatternBins(
      PatternIdxs(0, 2) -> MessageIdxs(),
      PatternIdxs(1)    -> MessageIdxs()
    )

    val tree = MatchingTree(MessageIdxs() -> bins)

    val patIdxs                             = MessageIdxs(0, 1, 2)
    val newMsg @ (newMsgIdx, newMsgMatches) = (0, PatternIdxs(0, 2))

    val newTree = updateMTree(tree, newMsgIdx, newMsgMatches)

    assert {
      newTree == tree ++ MatchingTree(
        MessageIdxs(newMsgIdx) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx),
            PatternIdxs(1)    -> MessageIdxs()
          )
      )
    }
  }

  test("Duplicate Types in pattern -- Multiple message tree update") {
    val bins = PatternBins(
      List(0, 2) -> MessageIdxs(),
      List(1)    -> MessageIdxs()
    )

    val tree = MatchingTree(MessageIdxs() -> bins)

    val patIdxs                                = MessageIdxs(0, 1, 2)
    val newMsg0 @ (newMsgIdx0, newMsgMatches1) = (0, PatternIdxs(0, 2))
    val newMsg1 @ (newMsgIdx1, newMsgMatches2) = (1, PatternIdxs(1))
    val newMsg2 @ (newMsgIdx2, newMsgMatches3) = (2, PatternIdxs(0, 2))

    val newTree0 = updateMTree(tree, newMsgIdx0, newMsgMatches1)
    val newTree1 = updateMTree(newTree0, newMsgIdx1, newMsgMatches2)
    val newTree2 = updateMTree(newTree1, newMsgIdx2, newMsgMatches3)

    assert {
      newTree0 == tree ++ MatchingTree(
        MessageIdxs(newMsgIdx0) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1)    -> MessageIdxs()
          )
      )
    }

    assert {
      newTree1 == tree ++ MatchingTree(
        MessageIdxs(newMsgIdx0, newMsgIdx1) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1)    -> MessageIdxs(newMsgIdx1)
          ),
        MessageIdxs(newMsgIdx0) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1)    -> MessageIdxs()
          ),
        MessageIdxs(newMsgIdx1) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(),
            PatternIdxs(1)    -> MessageIdxs(newMsgIdx1)
          )
      )
    }

    assert {
      newTree2 == tree ++ MatchingTree(
        MessageIdxs(newMsgIdx0, newMsgIdx1, newMsgIdx2) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0, newMsgIdx2),
            PatternIdxs(1)    -> MessageIdxs(newMsgIdx1)
          ),
        MessageIdxs(newMsgIdx0, newMsgIdx1) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1)    -> MessageIdxs(newMsgIdx1)
          ),
        MessageIdxs(newMsgIdx0, newMsgIdx2) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0, newMsgIdx2),
            PatternIdxs(1)    -> MessageIdxs()
          ),
        MessageIdxs(newMsgIdx1, newMsgIdx2) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx2),
            PatternIdxs(1)    -> MessageIdxs(newMsgIdx1)
          ),
        MessageIdxs(newMsgIdx0) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx0),
            PatternIdxs(1)    -> MessageIdxs()
          ),
        MessageIdxs(newMsgIdx1) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(),
            PatternIdxs(1)    -> MessageIdxs(newMsgIdx1)
          ),
        MessageIdxs(newMsgIdx2) ->
          PatternBins(
            PatternIdxs(0, 2) -> MessageIdxs(newMsgIdx2),
            PatternIdxs(1)    -> MessageIdxs()
          )
      )
    }
  }

  test("Find complete patterns in MatchingTree") {
    val mtree: MatchingTree = MatchingTree(
      MessageIdxs(1, 2)    -> PatternBins(PatternIdxs(1, 2) -> MessageIdxs(1, 2)),
      MessageIdxs(1, 2, 3) -> PatternBins(PatternIdxs(1, 2, 3) -> MessageIdxs(1, 2, 3))
    )

    val patternSize = 3

    val result = findCompletePatterns(mtree, patternSize)

    val expected: MatchingTree = MatchingTree(
      MessageIdxs(1, 2, 3) -> PatternBins(PatternIdxs(1, 2, 3) -> MessageIdxs(1, 2, 3))
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
      MessageIdxs(1, 2)       -> PatternBins(PatternIdxs(1, 2) -> MessageIdxs(1, 2)),
      MessageIdxs(1, 2, 3, 4) -> PatternBins(PatternIdxs(1, 2, 3, 4) -> MessageIdxs(1, 2, 3, 4))
    )
    val patternSize = 3
    val result      = findCompletePatterns(mtree, patternSize)
    assert(result.isEmpty)
  }

  test("Find complete patterns in MatchingTree -- Multiple complete patterns of given size") {
    val mtree: MatchingTree = MatchingTree(
      MessageIdxs(1, 2, 3) -> PatternBins(PatternIdxs(1, 2, 3) -> MessageIdxs(1, 2, 3)),
      MessageIdxs(4, 5, 6) -> PatternBins(PatternIdxs(4, 5, 6) -> MessageIdxs(4, 5, 6))
    )
    val patternSize = 3
    val result      = findCompletePatterns(mtree, patternSize)
    assert(result == mtree)
  }

  test("removeNode should remove the specified node from the tree") {
    val mtree: MatchingTree = MatchingTree(
      MessageIdxs(1, 2) -> PatternBins(PatternIdxs(1, 2) -> MessageIdxs(1, 2)),
      MessageIdxs(3, 4) -> PatternBins(PatternIdxs(3, 4) -> MessageIdxs(3, 4))
    )

    val messageIdxsToRemove = MessageIdxs(1, 2)

    val result = removeNode(mtree, messageIdxsToRemove)
    assert(
      result == MatchingTree(
        MessageIdxs(3, 4) -> PatternBins(PatternIdxs(3, 4) -> MessageIdxs(3, 4))
      )
    )
  }

  test(
    "Pruning a tree should remove nodes containing specified message indices -- Remove all nodes"
  ) {

    val prunedTree = pruneTree(exampleTree, MessageIdxs(0, 1, 2))

    assert(prunedTree.size == 1)
    assert(!prunedTree.contains(MessageIdxs(0)))
    assert(!prunedTree.contains(MessageIdxs(1)))
    assert(!prunedTree.contains(MessageIdxs(2)))
    assert(!prunedTree.contains(MessageIdxs(0, 1)))
    assert(!prunedTree.contains(MessageIdxs(0, 2)))
    assert(!prunedTree.contains(MessageIdxs(1, 2)))
    assert(!prunedTree.contains(MessageIdxs(0, 1, 2)))
  }

  test(
    "Pruning a tree should remove nodes containing specified message indices -- Remove some nodes"
  ) {

    val prunedTree = pruneTree(exampleTree, MessageIdxs(0, 1))

    assert(prunedTree.size == 2)
    assert(prunedTree.contains(MessageIdxs()))
    assert(prunedTree.contains(MessageIdxs(2)))
    assert(!prunedTree.contains(MessageIdxs(0)))
    assert(!prunedTree.contains(MessageIdxs(1)))
    assert(!prunedTree.contains(MessageIdxs(0, 1)))
    assert(!prunedTree.contains(MessageIdxs(0, 2)))
    assert(!prunedTree.contains(MessageIdxs(1, 2)))
  }

  test("Remove node from tree given message indices") {
    val updatedTree = removeNode(exampleTree, MessageIdxs(0, 1, 2))

    assert(updatedTree.size == 7)
    assert(!updatedTree.contains(MessageIdxs(0, 1, 2)))
    assert(updatedTree.contains(MessageIdxs()))
    assert(updatedTree.contains(MessageIdxs(0)))
    assert(updatedTree.contains(MessageIdxs(1)))
    assert(updatedTree.contains(MessageIdxs(2)))
    assert(updatedTree.contains(MessageIdxs(0, 1)))
    assert(updatedTree.contains(MessageIdxs(0, 2)))
    assert(updatedTree.contains(MessageIdxs(1, 2)))
  }
