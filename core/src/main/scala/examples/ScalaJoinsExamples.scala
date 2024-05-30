package join_patterns.examples
import joins.*

import scala.Console
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.*

import events.*

class JSize extends Joins:
  object A extends NullaryAsyncEvent
  object B extends NullaryAsyncEvent
  object C extends NullaryAsyncEvent
  object D extends NullaryAsyncEvent
  object E extends NullaryAsyncEvent
  object F extends NullaryAsyncEvent
  object G extends NullaryAsyncEvent

  private var i = 0

  join {
    case A() and1 B() and1 C() =>
      i += 1
      println(
        s"${Console.MAGENTA}${Console.UNDERLINED}JP 01 : Matched A, B, C -- $i${Console.RESET}"
      )
    case A() and1 B() and1 C() and1 D() =>
      i += 1
      println(
        s"${Console.MAGENTA}${Console.UNDERLINED}JP 02 : Matched A, B, C -- $i${Console.RESET}"
      )
    case A() and1 B() and1 C() and1 D() and1 E() =>
      i += 1
      println(
        s"${Console.MAGENTA}${Console.UNDERLINED}JP 03 : Matched A, B, C -- $i${Console.RESET}"
      )
    case A() and1 B() and1 C() and1 D() and1 E() and1 F() =>
      i += 1
      println(
        s"${Console.MAGENTA}${Console.UNDERLINED}JP 04 : Matched A, B, C, D, E, F -- $i${Console.RESET}"
      )
  }

  def finalMatchCount() = i

class JSizeWithGuards extends Joins:
  object A extends AsyncEvent[Int]
  object B extends AsyncEvent[Int]
  object C extends AsyncEvent[Int]
  object D extends AsyncEvent[Int]
  object E extends AsyncEvent[Int]
  object F extends AsyncEvent[Int]

  private var i = 0

  join {
    case A(x) and3 B(y) and3 C(z) and3 D(w) and3 E(v) and3 F(u) if x + y + z + w + v == u =>
      i += 1
      println(
        s"${Console.RED}${Console.UNDERLINED}JP 03 : Matched A($x), B($y), C($z), D($w), E($v), F($u) -- $i${Console.RESET}"
      )

    case A(x) and1 B(y) and1 C(z) if x + y == z =>
      i += 1
      println(
        s"${Console.RED}${Console.UNDERLINED}JP 01 : Matched A($x), B($y), C($z) -- $i${Console.RESET}"
      )

    case A(x) and2 B(y) and2 C(z) if x * y == z =>
      i += 1
      println(
        s"${Console.RED}${Console.UNDERLINED}JP 02 : Matched A($x), B($y), C($z) -- $i${Console.RESET}"
      )

  }

  def finalMatchCount() = i

object testJSize extends App:
  val jSize =
    new JSize:
      this.F()
      this.E()
      this.D()
      this.C()
      this.B()
      this.A()

  println(
    s"${Console.MAGENTA}JSize${Console.RESET}\t${Console.GREEN}final match count = ${jSize
        .finalMatchCount()}${Console.RESET}"
  )

  val jSizeWithGuards =
    new JSizeWithGuards:
      this.A(3)
      this.B(2)
      this.C(6)
      this.A(3)
      this.B(3)
      this.C(9)
      this.A(5)
      this.B(5)
      this.C(25)

  println(
    s"${Console.RED}JSizeWithGuards${Console.RESET}\t${Console.GREEN}final match count = ${jSizeWithGuards
        .finalMatchCount()}${Console.RESET}"
  )
