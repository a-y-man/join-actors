package join_patterns.util

import scala.quoted.*

def errorTree(using quotes: Quotes)(msg: String, token: quotes.reflect.Tree): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)

  token.symbol.pos match
    case Some(pos) => report.error(f"$msg: $t", pos)
    case None      => report.error(f"$msg: $t")

def errorTreeWithHint(using quotes: Quotes)(
    msg: String,
    hint: String,
    token: quotes.reflect.Tree
): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)
  val fullMsg = s"$msg: $t\n  Hint: $hint"

  token.symbol.pos match
    case Some(pos) => report.error(fullMsg, pos)
    case None      => report.error(fullMsg)

def macroAssert(using quotes: Quotes)(
    cond: Boolean,
    msg: String,
    token: quotes.reflect.Tree
): Unit =
  if !cond then errorTree(msg, token)

def error[T](using
    quotes: Quotes
)(msg: String, token: T, pos: Option[quotes.reflect.Position] = None): Unit =
  import quotes.reflect.*

  val show: String = token match
    case s: String => s
    case _         => token.toString

  pos match
    case Some(p) => report.error(f"$msg: $show", p)
    case None    => report.error(f"$msg: $show")
