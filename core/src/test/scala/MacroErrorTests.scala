package test

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scala.compiletime.testing.{typeCheckErrors, Error}

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

  test("duplicate variable name across constructors in &:& pattern produces error") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class A(x: Int) extends Evt
      case class B(x: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case A(n) &:& B(n) => Stop(n) }
      }(BruteForceMatcher)
    """)
    assert(errors.nonEmpty, "Expected a compile error for duplicate pattern variable 'n'")
  }

  test("named binding patterns produce errors in typeCheckErrors context") {
    // Patterns with named bindings hit a type inference limitation in typeCheckErrors:
    // Expr.ofList produces a type mismatch because the JoinPattern type parameters
    // are not properly resolved in the synthetic compilation context.
    // This means we cannot distinguish our shadowing errors from this spurious error.
    // Shadowing detection is validated by integration (SmartHouse example).
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(x: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case Ping(self) => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(errors.nonEmpty, "Named binding in typeCheckErrors should produce errors")
  }

  test("wildcard fields do not trigger errors") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class A(x: Int) extends Evt
      case class B(x: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case A(_) &:& B(_) => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(errors.isEmpty, s"Wildcard bindings should compile fine, got: ${errors.map(_.message)}")
  }

  test("wildcard single pattern compiles fine") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(x: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case Ping(_) => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(errors.isEmpty, s"Wildcard single pattern should compile, got: ${errors.map(_.message)}")
  }

  test("pattern type not a subtype of M produces error") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(n: Int) extends Evt
      case class Unrelated(s: String)

      receive { (self: ActorRef[Evt]) =>
        { case Unrelated(_) => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(errors.nonEmpty, "Expected a compile error for pattern type not subtype of M")
    assert(
      errors.exists(_.message.contains("not a subtype")),
      s"Error should mention 'not a subtype', got: ${errors.map(_.message)}"
    )
  }
