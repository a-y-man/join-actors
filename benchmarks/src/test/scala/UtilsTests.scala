package old_benchmarks

import old_benchmarks.GenerateGuardedSizeMsgs.*
import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UtilsTests extends AnyFunSuite with Matchers:
  test("genNNonMatchingMsgs generates correct number of messages") {
    val result = genNNonMatchingMsgs(3)(5)
    result.size should be(5)
  }

  test("genGuardedSizeMsgsOfSizeN generates correct size messages") {
    val result = genGuardedSizeMsgsOfSizeN(4)
    result.isDefined should be(true)
    result.get.size should be(4)
  }

  test("genGuardedSizeMsgsOfSizeN returns None for invalid size") {
    val result = genGuardedSizeMsgsOfSizeN(11)
    result.isDefined should be(false)
  }

  test("genNMatchingMsgSeqs generates correct number of matches") {
    val generator = (n: Int) => Seq.fill(n)("test")
    val result    = genNMatchingMsgSeqs(3)(generator)(5)
    result.size should be(15) // 3 * 5
  }

  test("genNMatchingMsgSeqs generates correct number of matches for empty generator") {
    val generator = (n: Int) => Seq.empty
    val result    = genNMatchingMsgSeqs(3)(generator)(5)
    result.size should be(0)
  }

  test("genNMatchingMsgSeqs generates correct number of matches for single match") {
    val generator = (n: Int) => Seq.fill(1)("test")
    val result    = genNMatchingMsgSeqs(3)(generator)(5)
    result.size should be(5)
  }
