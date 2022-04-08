package join_patterns

import java.util.concurrent.LinkedTransferQueue

sealed abstract class Msg
case class A() extends Msg
case class B() extends Msg
case class C() extends Msg
case class D() extends Msg
case class E(n: Int) extends Msg

@main
def main(): Unit =
	val i: Int = 0;
	val m = 8

	val iZero: () => Boolean = () => i == 0
	val iOne: () => Boolean = () => i != 0
	val isEight = ()
	val f = receive { (y: Msg) => y match
		/*
		case A() => 42
		case B() => 43
		case (A(), B()) => 44
		case _ => 45
		case (A(), _) => 46
		case (A(), B(), C()) => 47
		case D() if iZero() => 48
		*/
		case E(n: Int) => { { val n = "hi"; println(n) }; n + 1 }
	}

	// val test = receive { (n: Int) => n + 1 }dfsadf

	val q = LinkedTransferQueue[Msg]()

	q.add(A())
	val ret1 = f(q)
	println(f"After receiving A, f returned: ${ret1}")

	q.add(B())
	val ret2 = f(q)
	println(f"After receiving B, f returned: ${ret2}")

	q.add(D())
	val ret3 = f(q)
	println(f"After receiving D, f returned: ${ret3}")

	q.add(E(8))
	val ret4 = f(q)
	println(f"After receiving E(8), f returned: ${ret4}")

	/*
	q.add(A())
	q.add(B())
	val ret3 = f(q)
	println(f"After receiving A & B, f returned: ${ret3}")

	q.add(C())
	val ret4 = f(q)
	println(f"After receiving C, f returned: ${ret4}")

	q.add(A())
	q.add(C())
	val ret5 = f(q)
	println(f"After receiving A & C, f returned: ${ret5}")
	*/


