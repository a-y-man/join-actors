package join_patterns

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import java.util.concurrent.LinkedTransferQueue

class UnitTests extends AnyFunSuite {
  sealed abstract class Msg
  case class A() extends Msg
  case class B(n: Int) extends Msg
  case class C(n: String) extends Msg

  test("Single Empty Class, no predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case A() => result
    }
    val q = LinkedTransferQueue[Msg]()

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Single Empty Class, predicate") {
    val result = Random.nextInt
    val ifZero = (i: Int) => i == 0
    val rcv = receive { (y: Msg) => y match
      case A() if ifZero(1) => result + 1
      case A() if ifZero(0) => result
    }
    val q = LinkedTransferQueue[Msg]()

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Wildcard, no predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case _ => result
    }
    val q = LinkedTransferQueue[Msg]()

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
    val q = LinkedTransferQueue[Msg]()

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Single Class, One Int Member, no predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case B(n: Int) => n
    }
    val q = LinkedTransferQueue[Msg]()

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
    val q = LinkedTransferQueue[Msg]()

    q.add(B(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One String Member, no predicate") {
    val result = Random.nextString((Random.nextInt % 5) + 1)
    val rcv = receive { (y: Msg) => y match
      case C(n: String) => n
    }
    val q = LinkedTransferQueue[Msg]()

    q.add(C(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One String Member, predicate") {
    val result = Random.nextString((Random.nextInt % 5) + 1)
    val ifEmpty = (i: String) => i.isEmpty
    val rcv = receive { (y: Msg) => y match
      case C(n: String) if ifEmpty("") => n + Random.nextString(1)
      case C(n: String) if ifEmpty(n) => n
    }
    val q = LinkedTransferQueue[Msg]()

    q.add(C(result))

    assert(rcv(q) == result)
  }

  test("(Class(), Class())") {
    ???
  }

  test("Class(Int, String)") {
    ???
  }

  test("Class(Int, String) if guard()") {
    ???
  }
}