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
