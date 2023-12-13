package test.classes.sizeCount

import join_patterns.receive
import test.ALGORITHM
import test.benchmark.Benchmarkable
import test.classes.Msg

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

case class A() extends Msg
case class B() extends Msg
case class C() extends Msg
case class D() extends Msg
case class E() extends Msg
case class F() extends Msg
case class G() extends Msg
case class H() extends Msg
case class I() extends Msg
case class J() extends Msg

abstract class SizeCount(private val maxHits: Int) extends Benchmarkable[Msg, Unit]:
  @volatile var hits   = 0
  @volatile var isDone = false
  // var messages: ListBuffer[Msg] = ListBuffer()

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while !isDone do
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while !isDone do
      matcher(q)
      Thread.`yield`()

class Size1Count1(private val maxHits: Int) extends SizeCount(maxHits):
  protected val matcher = receive { (y: Msg) =>
    y match
      case A() =>
        hits += 1
        if hits >= maxHits then isDone = true
  }(ALGORITHM)

package object sizes:
  class Size2(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size3(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size4(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size5(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size6(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size7(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size8(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size9(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size10(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I(), J()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

package object sizesWithNoise:

  class Size1Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size2Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size3Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size4Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size5Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size6Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size7Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size8Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size9Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Size10Noise(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I(), J()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

package object counts:
  class Count2(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count3(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count4(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count5(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case E() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count6(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case E() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case F() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count7(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case E() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case F() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case G() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count8(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case E() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case F() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case G() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case H() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count9(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case E() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case F() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case G() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case H() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case I() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)

  class Count10(private val maxHits: Int) extends SizeCount(maxHits):
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case C() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case D() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case E() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case F() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case G() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case H() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case I() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case J() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }(ALGORITHM)
