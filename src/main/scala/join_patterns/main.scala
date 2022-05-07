package join_patterns

import java.util.concurrent.LinkedTransferQueue

sealed abstract class Msg
case class A() extends Msg
case class B() extends Msg
case class C() extends Msg
case class D() extends Msg
case class E(n: Int) extends Msg
case class F(a: Int, b: String) extends Msg

@main
def main(): Unit =
	val i: Int = 0;
	val m = 8

	val iZero: Int => Boolean = (n: Int) => n == 0

	val f = receive { (y: Msg) => y match
		/*
		case A() => 42
		case B() => 43
		case (A(), B()) => 44
		case (A(), _) => 46
		case (A(), B(), C()) => 47
		case D() if iZero() => 48
		case _ => 45
		*/
		case E(n: Int) if iZero(n) => { { val z = "hi"; println(z) }; n + 1 }
		case E(n: Int) => { { val z = "hi2"; println(z) }; n + 2 }
		//case F(a1: Int, b1: String) => b1.repeat(a1)
	}

	/*
	val g = receive {(m: List[Msg]) => Map[String, Any](
		("a": String) -> m(0).asInstanceOf[F]._1, ("b": String) -> m(0).asInstanceOf[F]._2
	)}
	*/
	/*
	q.add(A())
	val ret1 = f(q)
	println(f"After receiving A, f returned: ${ret1}")

	q.add(B())
	val ret2 = f(q)
	println(f"After receiving B, f returned: ${ret2}")

	q.add(E(6))
	val ret4 = f(q)
	println(f"After receiving E(tes), f returned: ${ret4}")

	q.add(F(3, "id"))
	val ret5 = f(q)
	println(f"After receiving F(3, 'id'), f returned: ${ret5}")
*/