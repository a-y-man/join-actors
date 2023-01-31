package test

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random
import java.util.concurrent.LinkedTransferQueue

import join_patterns.receive

abstract class UnitTests extends AnyFunSuite {
  sealed abstract class Msg
  case class A()                                      extends Msg
  case class B(n: Int)                                extends Msg
  case class C(n: String)                             extends Msg
  case class D()                                      extends Msg
  case class E()                                      extends Msg
  case class F(b: Int, a: String)                     extends Msg
  case class G(b: Int, a: String, c: Int, d: Boolean) extends Msg
}

class BaseFeatures extends UnitTests {
  test("Single Empty Message, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) =>
      y match
        case A() => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) === result)
  }

  test("Single Empty Message, Predicate") {
    val result = Random.nextInt
    val ifZero = (i: Int) => i == 0
    val rcv = receive { (y: Msg) =>
      y match
        case A() if ifZero(1) => result + 1
        case A() if ifZero(0) => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }
}

class MonoClassFields extends UnitTests {
  test("Single Message, One Int Member, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) =>
      y match
        case B(n: Int) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(B(result))

    assert(rcv(q) == result)
  }

  test("Single Message, One Int Member, Predicate") {
    val result = Random.nextInt
    val ifZero = (i: Int) => i == 0
    val rcv = receive { (y: Msg) =>
      y match
        case B(n: Int) if ifZero(1) => n + 1
        case B(n: Int) if ifZero(0) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(B(result))

    assert(rcv(q) == result)
  }

  test("Single Message, One String Member, no Predicate") {
    // val result = Random.alphanumeric.filter(_.isLetter).take((Random.nextInt % 5) + 1).mkString
    val result = "test"
    val rcv = receive { (y: Msg) =>
      y match
        case C(n: String) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(C(result))

    assert(rcv(q) == result)
  }

  test("Single Message, One String Member, Predicate") {
    // val result = Random.alphanumeric.filter(_.isLetter).take((Random.nextInt % 5) + 1).mkString
    val result     = "test"
    val ifNotEmpty = (i: String) => !i.isEmpty
    val rcv = receive { (y: Msg) =>
      y match
        case C(n: String) if ifNotEmpty("") =>
          n.appended(Random.alphanumeric.filter(_.isDigit).head)
        case C(n: String) if ifNotEmpty(n) => n
    }
    val q = LinkedTransferQueue[Msg]

    q.add(C(result))

    assert(rcv(q) == result)
  }

  test("Single Message, One Int and One String Members, no Predicate") {
    val result = "test "
    val rep    = Random.nextInt(5)
    val rcv = receive { (y: Msg) =>
      y match
        case F(z: Int, c: String) => c.repeat(z)
    }
    val q = LinkedTransferQueue[Msg]

    q.add(F(rep, result))

    assert(rcv(q) == result.repeat(rep))
  }

  test("Single Message, One Int and One String Members, Predicate") {
    val result                 = "test "
    val rep                    = Random.nextInt(5)
    val isZero: Int => Boolean = (n: Int) => n == 0
    val rcv = receive { (y: Msg) =>
      y match
        case F(z: Int, c: String) if isZero(z) => c
        case F(z: Int, c: String)              => c.repeat(z)
    }
    val q = LinkedTransferQueue[Msg]

    q.add(F(rep, result))

    assert(if rep == 0 then rcv(q) == result else rcv(q) == result.repeat(rep))
  }
}

class MultipleClasses extends UnitTests {
  test("Multiple Empty Messages, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) =>
      y match
        case (D(), A(), E()) => result
        case (A(), D())      => result + 1
        case (D(), E())      => result + 2
        case D()             => result + 3
        case E()             => result + 4
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())
    q.add(D())
    q.add(E())

    assert(rcv(q) == result + 1)
  }

  test("One Tupled Empty Message, no Predicate") {
    val result = Random.nextInt
    val rcv = receive { (y: Msg) =>
      y match
        case (A()) => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())

    assert(rcv(q) == result)
  }

  test("Multiple Empty Messages, Predicate") {
    val result                 = Random.nextInt
    val isZero: Int => Boolean = (n: Int) => n == 0
    val rcv = receive { (y: Msg) =>
      y match
        case (A(), D(), E()) if isZero(0) => result + 1
        case (A(), D(), E())              => result
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())
    q.add(D())
    q.add(E())

    assert(rcv(q) == result + 1)
  }

  test("Multiple Messages, One Int and One String Members, no Predicate") {
    val result = "test "
    val rep    = Random.nextInt(3)
    val rcv = receive { (y: Msg) =>
      y match
        case (F(i0: Int, s: String), D(), B(i1: Int)) => s.repeat(i0 + i1)
        case D()                                      => result
        case B(i: Int)                                => rep.toString
    }
    val q = LinkedTransferQueue[Msg]

    q.add(F(rep, result))
    q.add(D())
    q.add(B(rep))

    assert(rcv(q) == result.repeat(rep * 2))
  }

  test("Multiple Messages, One Int and One String Members, Predicate") {
    val result                     = "Hello World"
    val rep                        = Random.nextInt(3)
    val isEmpty: String => Boolean = (s: String) => s.isEmpty
    val rcv = receive { (y: Msg) =>
      y match
        case (F(i0: Int, s: String), D(), B(i1: Int)) if isEmpty(s) => "Hello World"
        case (F(i0: Int, s: String), D(), B(i1: Int)) if isEmpty(s) == false =>
          ("Hello " + s).repeat(i0 + i1)
        case B(i: Int) => rep.toString
    }
    val q = LinkedTransferQueue[Msg]

    q.add(F(rep, ""))
    q.add(D())
    q.add(B(rep))

    assert(rcv(q) == result)
  }
}

class Optionalfeatures extends UnitTests {
  // test("Wildcard, no Predicate") {
  //   val result = Random.nextInt
  //   val rcv = receive { (y: Msg) =>
  //     y match
  //       case _ => result
  //   }
  //   val q = LinkedTransferQueue[Msg]

  //   q.add(A())

  //   assert(rcv(q) == result)
  // }

  // test("Wildcard, Predicate") {
  //   val result = Random.nextInt
  //   val ifZero = (i: Int) => i == 0
  //   val rcv = receive { (y: Msg) =>
  //     y match
  //       case _ if ifZero(1) => result + 1
  //       case _ if ifZero(0) => result
  //   }
  //   val q = LinkedTransferQueue[Msg]

  //   q.add(A())

  //   assert(rcv(q) == result)
  // }

  // test("Multiple Messages of the same Class, One Int Member, no Predicate") {
  //   val (result0, result1) = (Random.nextInt, Random.nextInt)
  //   val rcv = receive { (y: Msg) =>
  //     y match
  //       case (B(i0: Int), B(i1: Int)) => i0 + i1
  //       case B(i: Int)                => i
  //   }
  //   val q = LinkedTransferQueue[Msg]

  //   q.add(B(result0))
  //   q.add(B(result1))

  //   assert(rcv(q) == result0 + result1)
  // }

  test("Multiple Messages of the same Class, One Int Member, Predicate") {
    val (result0, result1) = (Random.nextInt(Int.MaxValue - 1) + 1, Random.nextInt)
    val ifNotZero          = (i: Int) => i != 0
    val rcv = receive { (y: Msg) =>
      y match
        case (B(i0: Int), B(i1: Int), A()) if ifNotZero(result0) => i0 + i1
        case (B(i0: Int), B(i1: Int), A())                       => i0
        case B(i: Int)                                           => i
    }
    val q = LinkedTransferQueue[Msg]

    q.add(B(result0))
    q.add(A())
    q.add(B(result1))

    assert(rcv(q) == result0)
  }

  test("Wildcard field names, no Predicate") {
    val (result0, result1, result2) = (Random.nextInt, Random.nextInt.toString, Random.nextInt)
    val rcv = receive { (y: Msg) =>
      y match
        case G(_: Int, y: String, z: Int, _: Boolean) => y + z
    }
    val q = LinkedTransferQueue[Msg]

    q.add(G(result0, result1, result2, false))

    assert(rcv(q) == result1 + result2)
  }

  // test("Wildcard field names, Predicate") {
  //   val (result0, result1, result2, result3) =
  //     (Random.nextInt, Random.nextInt.toString, Random.nextInt, Random.nextBoolean)
  //   val is: Boolean => Boolean =
  //     (boolean: Boolean) => boolean
  //   val rcv = receive { (y: Msg) =>
  //     y match
  //       case G(_: Int, _: String, z: Int, b: Boolean) if is(b)  => z
  //       case G(y: Int, _: String, _: Int, b: Boolean) if is(!b) => y
  //   }
  //   val q = LinkedTransferQueue[Msg]

  //   q.add(G(result0, result1, result2, result3))

  //   assert(rcv(q) == (if result3 then result2 else result0))
  // }

  test("Dynamic join patterns") {
    val (result0, result1, result2, result3) =
      (Random.nextInt, Random.nextInt.toString, Random.nextInt, Random.nextBoolean)
    var rcv = receive { (y: Msg) =>
      y match
        case A() => result0
        case _   => result1
    }
    val q = LinkedTransferQueue[Msg]

    q.add(A())
    val outcome0 = rcv(q)

    rcv = receive { (y: Msg) =>
      y match
        case D() => result2
        case _   => result3
    }

    q.add(D())
    val outcome1 = rcv(q)

    assert(outcome0 == result0 && outcome1 == result2)
  }
}

// class JoinPatternOrdering extends UnitTests {
//   import join_patterns.JoinPattern
//   import join_patterns.receiveOrd

//   test("Patterns of size 2 can be reordered") {
//     val (result0, result1) = (Random.nextInt, Random.nextInt)
//     object PatternOrdering extends Ordering[JoinPattern[Msg, Int]] {
//       def compare(a: JoinPattern[Msg, Int], b: JoinPattern[Msg, Int]) = -1
//     }

//     val rcv0 = receiveOrd(PatternOrdering)({ (y: Msg) =>
//       y match
//         case (D(), E()) => result0
//         case (E(), D()) => result1
//     })

//     val rcv1 = receive { (y: Msg) =>
//       y match
//         case (D(), E()) => result0
//         case (E(), D()) => result1
//     }

//     val q = LinkedTransferQueue[Msg]

//     q.add(D())
//     q.add(E())

//     val resultReord = rcv0(q)

//     q.add(D())
//     q.add(E())

//     val resultNormal = rcv1(q)

//     assert(resultReord == result1 && resultNormal == result0)
//   }

//   test("Patterns of size 4 can be reordered") {
//     val results = (0 to 5).map(_ => Random.nextInt).toArray
//     val ifZero  = (i: Int) => i == 0
//     object PatternOrdering extends Ordering[JoinPattern[Msg, Int]] {
//       def compare(a: JoinPattern[Msg, Int], b: JoinPattern[Msg, Int]) =
//         if a.size % 2 == 1 then Int.MinValue
//         else if b.size % 2 == 1 then Int.MaxValue
//         else a.size.compare(b.size)
//     }

//     val rcv0 = receiveOrd(PatternOrdering)({ (y: Msg) =>
//       y match
//         case (A(), B(n: Int), D(), E())         => results(0)
//         case (B(n: Int), A(), D(), E())         => results(1)
//         case (A(), B(n: Int), D()) if ifZero(0) => results(2)
//         case (A(), E(), B(n: Int))              => results(3)
//         case (A(), B(n: Int))                   => results(4)
//         case (A(), D())                         => results(5)
//     })

//     val rcv1 = receive { (y: Msg) =>
//       y match
//         case (A(), B(n: Int), D(), E())         => results(0) //
//         case (B(n: Int), A(), D(), E())         => results(1)
//         case (A(), B(n: Int), D()) if ifZero(0) => results(2)
//         case (A(), E(), B(n: Int))              => results(3)
//         case (A(), B(n: Int))                   => results(4) //
//         case (A(), D())                         => results(5)
//     }

//     val q = LinkedTransferQueue[Msg]

//     q.add(A())
//     q.add(A())
//     q.add(B(1))
//     q.add(B(0))
//     q.add(D())
//     q.add(E())

//     val resultReord0 = rcv0(q)
//     val resultReord1 = rcv0(q)

//     q.add(A())
//     q.add(A())
//     q.add(B(1))
//     q.add(B(0))
//     q.add(D())
//     q.add(E())

//     val resultNormal0 = rcv1(q)
//     val resultNormal1 = rcv1(q)

//     assert(resultReord0 + resultReord1 == results(2) + results(3))
//     assert(resultNormal0 + resultNormal1 == results(0) + results(4))
//   }
// }
