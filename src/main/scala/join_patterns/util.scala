package join_patterns

import scala.quoted.*

def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] = '{ (x: T) =>
  ${ f('x) }
}

def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] = (x: Expr[T]) =>
  '{ $f($x) }

def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] =
  val code: String = expr.show
  Expr(code)

def _println[T](x: Expr[T])(using Quotes): Unit = {
  import quotes.reflect.*

  val tree: Tree = x.asTerm
  // println(tree.show(using Printer.TreeStructure))
  println(prettyPrint(tree))
}

def infoTree(using quotes: Quotes)(msg: String, token: quotes.reflect.Tree): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)

  token.symbol.pos match
    case Some(pos) => report.info(f"$msg: $t", pos)
    case None      => report.info(f"$msg: $t")

def errorTree(using quotes: Quotes)(msg: String, token: quotes.reflect.Tree): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)

  token.symbol.pos match
    case Some(pos) => report.error(f"$msg: $t", pos)
    case None      => report.error(f"$msg: $t")

def errorTypeRepr(using quotes: Quotes)(msg: String, token: quotes.reflect.TypeRepr): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TypeReprStructure)

  token.termSymbol.pos match
    case Some(pos) => report.error(f"$msg: $t", pos)
    case None      => report.error(f"$msg: $t")

def errorSig(using quotes: Quotes)(msg: String, token: quotes.reflect.Signature): Unit =
  import quotes.reflect.*

  report.error(f"$msg: ${token.paramSigs} => ${token.resultSig}")

def error[T](using
    quotes: Quotes
)(msg: String, token: T, pos: Option[quotes.reflect.Position] = None): Unit =
  import quotes.reflect.*

  val show: String = token match
    case s: String => s
    case _         => token.toString

  val _pos: Position = token match
    case _ if pos.isDefined => pos.get

  report.error(f"$msg: $show", _pos)
