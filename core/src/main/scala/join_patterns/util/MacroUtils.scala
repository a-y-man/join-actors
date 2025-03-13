package join_patterns.util

import scala.quoted.*

def errorTree(using quotes: Quotes)(msg: String, token: quotes.reflect.Tree): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)

  token.symbol.pos match
    case Some(pos) => report.error(f"$msg: $t", pos)
    case None      => report.error(f"$msg: $t")

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
