package test.macros

import join_actors.api.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import test.utils.*

import scala.concurrent.Await
import scala.concurrent.duration.*

sealed trait Ev
case class Num(x: Int) extends Ev
case class Other(y: Int) extends Ev
case class Lst(xs: List[Int]) extends Ev
case class Wide(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int)
    extends Ev

/** Runs `actor`, feeds it `msgs` and returns its result. */
def runActor[T](actor: Actor[Ev, T], msgs: Ev*): T =
  val (result, ref) = actor.start()
  msgs.foreach(ref ! _)
  Await.result(result, 10.seconds)

class MacroSubstitutionTests extends AnyFunSuite:

  test("fields of messages with 10 or more fields are bound to the right names") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, String] {
        receive { (self: ActorRef[Ev]) =>
          { case Wide(a, b, c, d, e, f, g, h, i, j, k, l) =>
            Stop(List(a, b, c, d, e, f, g, h, i, j, k, l).mkString(","))
          }
        }(matcher)
      }
      runActor(actor, Wide(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) shouldBe "1,2,3,4,5,6,7,8,9,10,11,12"
    }
  }

  test("guards see the right fields of messages with 10 or more fields") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Wide(_, _, _, _, _, _, _, _, _, j, _, l) if j == 10 && l == 12 => Stop(j + l) }
        }(matcher)
      }
      runActor(actor, Wide(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) shouldBe 22
    }
  }

  // Pattern variables are identified by symbol, so a same-named inner definition is not mistaken
  // for them. Each case below used to be either rejected by the macro or, worse, silently wrong.

  test("a local val that shadows a pattern variable keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) =>
            val y = x
            val r = { val x = 7; x + y }
            Stop(r)
          }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 8
    }
  }

  test("a local def that shadows a pattern variable keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) =>
            val y = x
            def z = { def x = 7; x + y }
            Stop(z)
          }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 8
    }
  }

  test("a lambda parameter that shadows a pattern variable keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) => Stop(List(10, 20).map(x => x + 1).sum + x) }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 33
    }
  }

  test("a nested pattern variable that shadows a pattern variable keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) => Stop(Option(5) match { case Some(x) => x; case None => 0 }) }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 5
    }
  }

  test("a for-comprehension variable that shadows a pattern variable keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) => Stop((for x <- List(1, 2) yield x * 10).sum + x) }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 31
    }
  }

  test("a parameter of a local def that shadows a pattern variable keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, String] {
        receive { (self: ActorRef[Ev]) =>
          { case Lst(xs) =>
            def f(xs: Int, ys: Int) = xs + ys
            Stop(s"$xs ${f(ys = 1, xs = 2)}")
          }
        }(matcher)
      }
      runActor(actor, Lst(List(1, 2))) shouldBe "List(1, 2) 3"
    }
  }

  test("a lambda parameter named like the self parameter keeps its own value") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) => Stop(List(1, 2).map(self => self * 2).sum + x) }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 7
    }
  }

  test("a pattern variable named like the self parameter refers to the pattern variable") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(self) => Stop(self) }
        }(matcher)
      }
      runActor(actor, Num(4)) shouldBe 4
    }
  }

  test("self still refers to the actor in the body") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          {
            case Num(x) =>
              self ! Other(x + 1)
              Continue
            case Other(y) => Stop(y)
          }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 2
    }
  }

  test("self is available in the body of a wildcard case") {
    forAll(matchers) { matcher =>
      // Only checks that the expansion compiles and the actor can be built: before, `self` in a
      // wildcard body escaped the scope where it was defined.
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case _ =>
            self ! Num(1)
            Stop(0)
          }
        }(matcher)
      }
      actor should not be null
    }
  }

  test("an outer definition with the same name as a pattern variable is not an error") {
    val x = 100
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) => { case Num(x) => Stop(x) } }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 1
    }
  }

  test("an outer local value that is not shadowed can be used in the body and the guard") {
    val offset = 100
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) => { case Num(x) if x < offset => Stop(x + offset) } }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 101
    }
  }

  test("a lambda parameter in a guard that shadows a variable of another message") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) &:& Other(y) if List(1, 2).exists(y => y == x) => Stop(x + y) }
        }(matcher)
      }
      runActor(actor, Num(2), Other(9)) shouldBe 11
    }
  }

  test("a lambda parameter in a guard that shadows a variable of the same message") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) if List(1, 2).exists(x => x == 2) => Stop(x) }
        }(matcher)
      }
      runActor(actor, Num(0)) shouldBe 0
    }
  }

  test("a guard clause with a shadowing lambda does not confuse per-message filtering") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) &:& Other(y) if x > 0 && List(2).exists(y => y == 2) && y == 5 => Stop(x + y) }
        }(matcher)
      }
      runActor(actor, Num(1), Other(4), Other(5)) shouldBe 6
    }
  }

  test("the same variable name in different cases") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          {
            case Num(x) if x > 5 => Stop(x)
            case Other(x) =>
              val y = x * 2
              Stop(-y)
          }
        }(matcher)
      }
      runActor(actor, Other(4)) shouldBe -8
    }
  }

  test("a closure that captures a pattern variable sees its value later") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) =>
            val later = () => x + 1
            Stop(later())
          }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 2
    }
  }

  test("pattern variables can be passed to inline methods") {
    forAll(matchers) { matcher =>
      val actor = Actor[Ev, Int] {
        receive { (self: ActorRef[Ev]) =>
          { case Num(x) =>
            require(x > 0)
            Stop(math.max(x, 3).min(5) + scala.math.abs(x))
          }
        }(matcher)
      }
      runActor(actor, Num(1)) shouldBe 4
    }
  }
