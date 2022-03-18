package join_patterns

import scala.quoted.*

def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = '{(x: T) => ${f('x)}}

def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] =	(x: Expr[T]) => '{$f($x)}

inline def assert(inline expr: Boolean): Unit = ${assertImpl('expr)}

def assertImpl(expr: Expr[Boolean])(using Quotes): Expr[Unit] = '{
	if !$expr then
		println(s"failed assertion: ${${showExpr(expr)}}")
}

def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] =
	val code: String = expr.show
	Expr(code)

def _out(msg: String): String = {
	s"[Join-Patterns] ${msg}"
}

def _err(msg: String): String = {
	_out(s"Error: ${msg}")
}
