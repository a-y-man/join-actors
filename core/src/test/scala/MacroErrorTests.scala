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

  test("pattern variable named like the self parameter is allowed") {
    // Pattern variables are identified by symbol, so shadowing `self` is unambiguous: inside the
    // case, `self` is the pattern variable.
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(x: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case Ping(self) => Stop(self) }
      }(BruteForceMatcher)
    """)
    assert(errors.isEmpty, s"Shadowing self should compile, got: ${errors.map(_.message)}")
  }

  test("guard that refers to self produces a clear error") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Ping(x: Int) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case Ping(x) if self != null => Stop(x) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.exists(_.message.contains("guard of a join pattern cannot refer to `self`")),
      s"Expected an error about self in a guard, got: ${errors.map(_.message)}"
    )
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

  test("user-defined extractor is rejected instead of being silently ignored") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class N(x: Int) extends Evt
      object Even { def unapply(n: N): Option[Int] = if n.x % 2 == 0 then Some(n.x) else None }

      receive { (self: ActorRef[Evt]) =>
        { case Even(x) => Stop(x) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("Unsupported extractor"),
      s"Expected exactly one error mentioning 'Unsupported extractor', got: ${errors.map(_.message)}"
    )
  }

  test("hand-written unapply in the companion of a case class is rejected") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class N(x: Int) extends Evt
      object N { def unapply(n: N): Option[Int] = Some(n.x + 1) }

      receive { (self: ActorRef[Evt]) =>
        { case N(x) => Stop(x) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("Unsupported extractor"),
      s"Expected exactly one error mentioning 'Unsupported extractor', got: ${errors.map(_.message)}"
    )
  }

  test("sequence pattern is rejected with a message") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class V(xs: Int*) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case V(xs*) => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("Unsupported extractor"),
      s"Expected exactly one error mentioning 'Unsupported extractor', got: ${errors.map(_.message)}"
    )
  }

  test("generic message class is rejected with a message") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class Box[T](v: T) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case Box(v) => Stop(()) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("type parameters"),
      s"Expected exactly one error mentioning 'type parameters', got: ${errors.map(_.message)}"
    )
  }

  test("type test on a payload is rejected instead of being silently ignored") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class P(x: Any) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case P(x: Int) => Stop(x) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("Unsupported type test"),
      s"Expected exactly one error mentioning 'Unsupported type test', got: ${errors.map(_.message)}"
    )
  }

  test("type test in a wildcard payload is rejected") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class P(x: Any) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case P(_: Int) => Stop(1) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("Unsupported type test"),
      s"Expected exactly one error mentioning 'Unsupported type test', got: ${errors.map(_.message)}"
    )
  }

  test("nested constructor pattern gives a single error") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      case class In(a: Int)
      case class Out(i: In) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case Out(In(a)) => Stop(a) }
      }(BruteForceMatcher)
    """)
    assert(
      errors.size == 1 && errors.head.message.contains("Unsupported payload binding"),
      s"Expected exactly one error mentioning 'Unsupported payload binding', got: ${errors.map(_.message)}"
    )
  }

  test("payload typed with its declared type compiles") {
    val errors = typeCheckErrors("""
      import join_actors.api.*
      import join_actors.actor.Result.Stop

      sealed trait Evt
      type Id = Int
      case class P(x: Int, y: List[String], z: Id) extends Evt

      receive { (self: ActorRef[Evt]) =>
        { case P(x: Int, y: List[String], z: Int) => Stop(x) }
      }(BruteForceMatcher)
    """)
    assert(errors.isEmpty, s"Declared payload types should compile, got: ${errors.map(_.message)}")
  }
