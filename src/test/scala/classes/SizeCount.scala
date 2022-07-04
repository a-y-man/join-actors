package test.classes.sizeCount

import scala.concurrent.{Future, ExecutionContext}
import scala.collection.mutable.ListBuffer

import join_patterns.receive
import test.classes.Msg
import test.benchmark.Benchmarkable

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

abstract class SizeCount(private val maxHits: Int) extends Benchmarkable[Msg, Unit] {
  var hits                      = 0
  var isDone                    = false
  var messages: ListBuffer[Msg] = ListBuffer()

  def messageMatching: () => Boolean

  def run_as_future: Future[Long] =
    implicit val ec = ExecutionContext.global

    Future {
      val start = System.nanoTime

      while !isDone do
        matcher(q)
        Thread.`yield`()

      System.nanoTime - start
    }

  def run_without_macro: Future[Long] =
    implicit val ec = ExecutionContext.global
    import collection.convert.ImplicitConversions._

    Future {
      val start = System.nanoTime

      while !isDone do
        if messageMatching() then
          hits += 1
          if hits >= maxHits then isDone = true
        else
          messages.append(q.take())
          q.drainTo(messages)

        Thread.`yield`()

      System.nanoTime - start
    }

  override def run =
    while !isDone do
      matcher(q)
      Thread.`yield`()
}

class Size1Count1(private val maxHits: Int) extends SizeCount(maxHits) {
  protected val matcher = receive { (y: Msg) =>
    y match
      case A() =>
        hits += 1
        if hits >= maxHits then isDone = true
  }

  override def messageMatching = () => q.take.isInstanceOf[A]
}

package object sizes {
  class Size2(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size3(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size4(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size5(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D]),
        messages.find(_.isInstanceOf[E])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size6(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D]),
        messages.find(_.isInstanceOf[E]),
        messages.find(_.isInstanceOf[F])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size7(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D]),
        messages.find(_.isInstanceOf[E]),
        messages.find(_.isInstanceOf[F]),
        messages.find(_.isInstanceOf[G])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size8(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D]),
        messages.find(_.isInstanceOf[E]),
        messages.find(_.isInstanceOf[F]),
        messages.find(_.isInstanceOf[G]),
        messages.find(_.isInstanceOf[H])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size9(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D]),
        messages.find(_.isInstanceOf[E]),
        messages.find(_.isInstanceOf[F]),
        messages.find(_.isInstanceOf[G]),
        messages.find(_.isInstanceOf[H]),
        messages.find(_.isInstanceOf[I])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }

  class Size10(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case (A(), B(), C(), D(), E(), F(), G(), H(), I(), J()) =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () =>
      val search = List(
        messages.find(_.isInstanceOf[A]),
        messages.find(_.isInstanceOf[B]),
        messages.find(_.isInstanceOf[C]),
        messages.find(_.isInstanceOf[D]),
        messages.find(_.isInstanceOf[E]),
        messages.find(_.isInstanceOf[F]),
        messages.find(_.isInstanceOf[G]),
        messages.find(_.isInstanceOf[H]),
        messages.find(_.isInstanceOf[I]),
        messages.find(_.isInstanceOf[J])
      )

      if search.forall(_.isDefined) then
        messages.subtractAll(search.map(_.get))
        true
      else false
  }
}

package object counts {
  class Count2(private val maxHits: Int) extends SizeCount(maxHits) {
    protected val matcher = receive { (y: Msg) =>
      y match
        case A() =>
          hits += 1
          if hits >= maxHits then isDone = true
        case B() =>
          hits += 1
          if hits >= maxHits then isDone = true
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B]
  }

  class Count3(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C]
  }

  class Count4(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D]
  }

  class Count5(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D | E]
  }

  class Count6(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D | E | F]
  }

  class Count7(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D | E | F | G]
  }

  class Count8(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D | E | F | G | H]
  }

  class Count9(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D | E | F | G | H | I]
  }

  class Count10(private val maxHits: Int) extends SizeCount(maxHits) {
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
    }

    override def messageMatching = () => q.take.isInstanceOf[A | B | C | D | E | F | G | H | I | J]
  }
}
