package join_patterns

import java.util.concurrent.LinkedTransferQueue

sealed abstract class Msg
case class A() extends Msg
case class B() extends Msg
case class C() extends Msg
case class D() extends Msg
case class E(n: Int) extends Msg
case class F(b: Int, a: String) extends Msg

@main
def main(): Unit =
	val i: Int = 0;
	val m = 0
	val isZero: Int => Boolean = (n: Int) => n == 0
	val q = LinkedTransferQueue[Msg]

	val f = receive { (y: Msg) => y match
		/*
		case A() => 42
		case B() => 43
		case D() if iZero() => 48
		case _ => 45
		case E(n: Int) if isZero(n) => { { val z = "hi"; println(z) }; n + 1 }
		case E(n: Int) => { { val z = "hi2"; println(z) }; n + 2 }
		case F(z: Int, c: String) if isZero(z) => "Hello " + c + "! "
		case F(z: Int, c: String) => ("Hello " + c + "! ").repeat(z)
		*/
		case (A(), B()) => 44
		//case (A(), B(), C()) => 47
		//case (A(), _) => 46
	}
/*
	val g = receive {
		(m: List[Msg]) =>
			m.exists(_.isInstanceOf[A]) && m.exists(_.isInstanceOf[B]) && m.exists(_.isInstanceOf[C])
	}
	*/
/*
	q.add(A())
	val ret1 = f(q)
	println(f"After receiving A, f returned: ${ret1}")

	q.add(B())
	val ret2 = f(q)
	println(f"After receiving B, f returned: ${ret2}")

	q.add(F(0, "Antoine"))
	val ret5 = f(q)
	println(f"After receiving F(3, 'id'), f returned: ${ret5}")
*/
	q.add(A())
	q.add(B())
	val ret1 = f(q)
	println(f"After receiving (A, B), f returned: ${ret1}")
