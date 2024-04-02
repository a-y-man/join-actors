package core

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.examples.*
import join_patterns.receive

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App:
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

  // val bbConfig = BBConfig(
  //   bufferBound = 2,
  //   producers = 4,
  //   consumers = 4,
  //   cnt = 2,
  //   algorithm = MatchingAlgorithm.StatefulTreeBasedAlgorithm
  // )
  // runBB(bbConfig)

  santaClausExample(MatchingAlgorithm.StatefulTreeBasedAlgorithm, 4)
  santaClausExample(MatchingAlgorithm.BruteForceAlgorithm, 4)

  // chameneosExample(
  //   maxNumberOfMeetings = 5,
  //   numberOfChameneos = 5,
  //   algorithm = MatchingAlgorithm.StatefulTreeBasedAlgorithm
  // )
  // chameneosExample(
  //   maxNumberOfMeetings = 5,
  //   numberOfChameneos = 5,
  //   algorithm = MatchingAlgorithm.BruteForceAlgorithm
  // )
