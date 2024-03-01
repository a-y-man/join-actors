package join_patterns

import join_patterns.examples.*

@main
def main(): Unit =
  // demo(MatchingAlgorithm.BruteForceAlgorithm)
  // demo(MatchingAlgorithm.StatefulTreeBasedAlgorithm)

  // test01(MatchingAlgorithm.BruteForceAlgorithm)
  // test01(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test02(MatchingAlgorithm.BruteForceAlgorithm)
  // test02(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test03(MatchingAlgorithm.BruteForceAlgorithm)
  // test03(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test04(MatchingAlgorithm.BruteForceAlgorithm)
  // test04(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test05(MatchingAlgorithm.BruteForceAlgorithm)
  // test05(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test06(MatchingAlgorithm.BruteForceAlgorithm)
  // test06(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test07(MatchingAlgorithm.BruteForceAlgorithm)
  // test07(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // test08(MatchingAlgorithm.BruteForceAlgorithm)
  // test08(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // randomMsgTesting(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // randomMsgTesting(MatchingAlgorithm.BruteForceAlgorithm)

  // smartHouseExample(MatchingAlgorithm.StatefulTreeBasedAlgorithm, 10)
  // smartHouseExample(MatchingAlgorithm.BruteForceAlgorithm, 10)
  // nwptExample(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // nwptExample(MatchingAlgorithm.BruteForceAlgorithm)
  // pingPongExample(4, MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  // pingPongExample(4, MatchingAlgorithm.BruteForceAlgorithm)
  boundedBufferExample(MatchingAlgorithm.BruteForceAlgorithm, 10)
  boundedBufferExample(MatchingAlgorithm.StatefulTreeBasedAlgorithm, 10)

// [{} -> [{0, 2} -> {}, {1} -> {}]
// {0} -> [{0, 2} -> {}, {1} -> {}]
// {1} -> [{0, 2} -> {}, {1} -> {}]
// {2} -> [{0, 2} -> {}, {1} -> {}]
// {1, 2} -> [{0, 2} -> {1}, {1} -> {}]
// {0, 1} -> [{0, 2} -> {}, {1} -> {0}]
// {0, 2} -> [{0, 2} -> {0}, {1} -> {}]
// {0, 1, 2} -> [{0, 2} -> {0, 1}, {1} -> {0}]]
