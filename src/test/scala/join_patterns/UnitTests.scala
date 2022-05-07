package join_patterns

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import java.util.concurrent.LinkedTransferQueue

/*
Does not work if placed inside the class. Maybe a bug ?
*/
sealed abstract class Msg
case class A() extends Msg
case class B(n: Int) extends Msg
case class C(n: String) extends Msg

class UnitTests extends AnyFunSuite {

  test("Single Empty Class, no predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case A() => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) === result)
  }

  test("Single Empty Class, predicate") {
    val result = Random.nextInt
    val ifZero = (i: Int) => i == 0
    val rcv = receive { (y: Msg) => y match
      case A() if ifZero(1) => result + 1
      case A() if ifZero(0) => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Wildcard, no predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case _ => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Wildcard, predicate") {
    val result = Random.nextInt
    val ifZero = (i: Int) => i == 0
    val rcv = receive { (y: Msg) => y match
      case _ if ifZero(1) => result + 1
      case _ if ifZero(0) => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Single Class, One Int Member, no predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case B(n: Int) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(B(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One Int Member, predicate") {
    val result = Random.nextInt
    val ifZero = (i: Int) => i == 0
    val rcv = receive { (y: Msg) => y match
      case B(n: Int) if ifZero(1) => n + 1
      case B(n: Int) if ifZero(0) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(B(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One String Member, no predicate") {
    //val result = Random.alphanumeric.filter(_.isLetter).take((Random.nextInt % 5) + 1).mkString
    val result = "test"
    val rcv = receive { (y: Msg) => y match
      case C(n: String) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(C(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One String Member, predicate") {
    //val result = Random.alphanumeric.filter(_.isLetter).take((Random.nextInt % 5) + 1).mkString
    val result = "test"
    val ifNotEmpty = (i: String) => !i.isEmpty
    val rcv = receive { (y: Msg) => y match
      case C(n: String) if ifNotEmpty("") => n.appended(Random.alphanumeric.filter(_.isDigit).head)
      case C(n: String) if ifNotEmpty(n) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(C(result))

    assert(rcv(q) == result)
  }
/*
  test("(Class(), Class())") {
    ???
  }

  test("Class(Int, String)") {
    ???
  }

  test("Class(Int, String) if guard()") {
    ???
  }
  */
}