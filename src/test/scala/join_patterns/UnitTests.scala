package join_patterns

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import java.util.concurrent.LinkedTransferQueue

class UnitTests extends AnyFunSuite {
  sealed abstract class Msg
  case class A() extends Msg
  case class B(n: Int) extends Msg
  case class C(n: String) extends Msg
  case class D() extends Msg
  case class E() extends Msg
  case class F(b: Int, a: String) extends Msg

  test("Single Empty Class, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case A() => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) === result)
  }

  test("Single Empty Class, Predicate") {
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

  test("Wildcard, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case _ => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Wildcard, Predicate") {
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

  test("Single Class, One Int Member, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case B(n: Int) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(B(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One Int Member, Predicate") {
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

  test("Single Class, One String Member, no Predicate") {
    //val result = Random.alphanumeric.filter(_.isLetter).take((Random.nextInt % 5) + 1).mkString
    val result = "test"
    val rcv = receive { (y: Msg) => y match
      case C(n: String) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(C(result))

    assert(rcv(q) == result)
  }

  test("Single Class, One String Member, Predicate") {
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

  test("Single Class, One Int and One String Members, no Predicate") {
    val result = "test "
    val rep = Random.nextInt(5)
    val rcv = receive { (y: Msg) => y match
      case F(z: Int, c: String) => c.repeat(z)
    }
    val q = LinkedTransferQueue[Msg]

    q.add(F(rep, result))

    assert(rcv(q) == result.repeat(rep))
  }

  test("Single Class, One Int and One String Members, Predicate") {
    val result = "test "
    val rep = Random.nextInt(5)
    val isZero: Int => Boolean = (n: Int) => n == 0
    val rcv = receive { (y: Msg) => y match
      case F(z: Int, c: String) if isZero(z) => c
      case F(z: Int, c: String) => c.repeat(z)
    }
    val q = LinkedTransferQueue[Msg]

    q.add(F(rep, result))

    assert(if rep == 0 then rcv(q) == result else rcv(q) == result.repeat(rep))
  }

  // BUG result + 3
  test("Multiple Empty Classes, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case (A(), D(), E()) => result
      case (A(), D()) => result + 1
      case (D(), E()) => result + 2
      case D() => result + 3
      case E() => result + 4
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())
    q.add(D())
    q.add(E())

    assert(rcv(q) == result)
  }

  test("One Tupled Empty Class, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) => y match
      case (A()) => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Multiple Empty Classes, Predicate") {
    val result = Random.nextInt
    val isZero: Int => Boolean = (n: Int) => n == 0
    val rcv = receive { (y: Msg) => y match
      case (A(), D(), E()) if isZero(0) => result + 1
      case (A(), D(), E()) => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())
    q.add(D())
    q.add(E())

    assert(rcv(q) == result + 1)
  }
}