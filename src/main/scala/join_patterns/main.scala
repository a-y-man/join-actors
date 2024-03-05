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
  boundedBufferExample(MatchingAlgorithm.StatefulTreeBasedAlgorithm, 10)
  // boundedBufferExample(MatchingAlgorithm.BruteForceAlgorithm, 10)

// M = [ 3, 4, 5 ]  P = [ 0, 1, 2 ]

// 3 -> { 0, 2 }
// 4 -> { 1 }
// 5 -> { 0, 2 }

// { 3, 4, 5 } -> [{ 0, 2 } -> { 3, 5 }, { 1 } -> { 4 }]]
// { 3, 4 }    -> [{ 0, 2 } -> { 3 },    { 1 } -> { 4 }]
// { 3, 5 }    -> [{ 0, 2 } -> { 3, 5 }, { 1 } -> {   }]
// { 4, 5 }    -> [{ 0, 2 } -> { 5 },    { 1 } -> { 4 }]
// { 3 }       -> [{ 0, 2 } -> { 3 },    { 1 } -> {   }]
// { 4 }       -> [{ 0, 2 } -> {   },    { 1 } -> { 4  }]
// { 5 }       -> [{ 0, 2 } -> { 5 },    { 1 } -> {   }]
// [{ }        -> [{ 0, 2 } -> {   },    { 1 } -> {   }]
