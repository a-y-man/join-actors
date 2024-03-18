package core

import actor.*
import join_patterns.MatchingAlgorithm
import join_patterns.examples.*
import join_patterns.receive

import scala.concurrent.Await
import scala.concurrent.duration.Duration

sealed trait SizeMsg
case class A()         extends SizeMsg
case class B()         extends SizeMsg
case class C()         extends SizeMsg
case class D()         extends SizeMsg
case class E()         extends SizeMsg
case class F()         extends SizeMsg
case class G()         extends SizeMsg
case class H()         extends SizeMsg
case class I()         extends SizeMsg
case class J()         extends SizeMsg
case class Terminate() extends SizeMsg

def size10(algorithm: MatchingAlgorithm) =
  val act = Actor[SizeMsg, Long] {
    receive { (y: SizeMsg, _: ActorRef[SizeMsg]) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I(), J()) =>
          Next()
        case Terminate() =>
          Stop(System.currentTimeMillis())
    }(algorithm)
  }

  act

object Main extends App:

  def runSize10(algorithm: MatchingAlgorithm) =
    val startTime     = System.currentTimeMillis()
    val (actFut, act) = size10(algorithm).start()
    // act ! A()
    act ! B()
    act ! C()
    act ! D()
    act ! E()
    act ! F()
    act ! G()
    act ! H()
    act ! I()
    act ! J()
    act ! A()
    act ! Terminate()
    val endTime = Await.result(actFut, Duration.Inf)
    println(s"${algorithm}: ${endTime - startTime}ms")

  runSize10(MatchingAlgorithm.BruteForceAlgorithm)
  runSize10(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  runSize10(MatchingAlgorithm.BruteForceAlgorithm)
  runSize10(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  runSize10(MatchingAlgorithm.BruteForceAlgorithm)
  runSize10(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  runSize10(MatchingAlgorithm.BruteForceAlgorithm)
  runSize10(MatchingAlgorithm.StatefulTreeBasedAlgorithm)
  runSize10(MatchingAlgorithm.BruteForceAlgorithm)
  runSize10(MatchingAlgorithm.StatefulTreeBasedAlgorithm)

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
  // boundedBufferExample(MatchingAlgorithm.StatefulTreeBasedAlgorithm, 10)
  // boundedBufferExample(MatchingAlgorithm.BruteForceAlgorithm, 10)
