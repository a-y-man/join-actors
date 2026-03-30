package test

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scala.compiletime.testing.{typeCheckErrors, Error}

/** Compile-time negative tests verifying that the `receive` macro
  * produces helpful error messages for invalid pattern syntax.
  *
  * Uses `scala.compiletime.testing.typeCheckErrors` to capture macro errors
  * without failing compilation.
  */
class MacroErrorTests extends AnyFunSuite:

  test("string literal pattern produces error") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(n: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case "not a pattern" => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(errors.nonEmpty, "Expected a compile error for string literal pattern")
  }

  test("non-case-class pattern produces error with hint") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(n: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case 42 => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(errors.nonEmpty, "Expected a compile error for integer literal pattern")
  }
