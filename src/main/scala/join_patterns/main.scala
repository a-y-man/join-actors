package join_patterns

import java.util.concurrent.LinkedTransferQueue

sealed abstract class Msg
case class A()                                      extends Msg
case class B()                                      extends Msg
case class C()                                      extends Msg
case class D()                                      extends Msg
case class E(n: Int)                                extends Msg
case class F(b: Int, a: String)                     extends Msg
case class G(b: Int, a: String, c: Int, d: Boolean) extends Msg

object PatternOrdering extends Ordering[JoinPattern[Msg, Int]] {
  def compare(a: JoinPattern[Msg, Int], b: JoinPattern[Msg, Int]) = a.size.compare(b.size)
}

@main
def main(): Unit =
  val i: Int                 = 0;
  val m                      = 0
  val isZero: Int => Boolean = (n: Int) => n == 0
  val q                      = LinkedTransferQueue[Msg]

  var f = receive { (y: Msg) =>
    y match
      case A() => 5
    /*
		case E(n: Int) if isZero(n) => { { val z = "hi"; println(z) }; n + 1 }
		case E(n: Int) => { { val z = "hi2"; println(z) }; n + 2 }
		case F(z: Int, c: String) if isZero(z) => "Hello " + c + "! "
    case (G(_: Int, _: String, z: Int, bool: Boolean), F(b: Int, _: String)) => z + b
     */
    // case (E(a: Int), E(b: Int), C()) => a + b
    // case (A(), _) => 46
    // case (A(), A()) => 46
  }
  /*
	val g = receive {
		(m: List[Msg]) =>
			m.exists(_.isInstanceOf[A]) && m.exists(_.isInstanceOf[B]) && m.exists(_.isInstanceOf[C])
	}
   */
  q.add(A())
  /*
	val ret1 = f(q)
	println(f"After receiving A, f returned: ${ret1}")

	q.add(F(0, "Antoine"))
	val ret5 = f(q)
	println(f"After receiving F(3, 'id'), f returned: ${ret5}")
  q.add(G(2, "t", -5, false))
  q.add(F(2, "t"))
   */
  val ret1 = f(q)
  println(f"After receiving A, f returned: ${ret1}")
