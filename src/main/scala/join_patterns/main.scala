package join_patterns

import java.util.concurrent.LinkedTransferQueue

sealed abstract class Msg
case class A() extends Msg
case class B() extends Msg
case class C() extends Msg

@main
def main(): Unit =
	val i: Int = 0;

	val iZero: Int => Boolean = (i: Int) => i == 0
	val iOne: Int => Boolean = (i: Int) => i != 0
	val f = receive { (y: Msg) => y match
		case A() => 42
		case B() => 43
		case (A(), B()) => 44
		case _ => 45
		case (A(), _) => 46
		case (A(), B(), C()) => 47
	}

	val q = LinkedTransferQueue[Msg]()

	q.add(A())
	val ret1 = f(q)
	println(f"After receiving A, f returned: ${ret1}")

	q.add(B())
	val ret2 = f(q)
	println(f"After receiving B, f returned: ${ret2}")

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

