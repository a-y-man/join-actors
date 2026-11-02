package join_patterns.util

import scala.quoted.*

def errorTree(using quotes: Quotes)(msg: String, token: quotes.reflect.Tree): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)
  report.error(f"$msg: $t", token.pos)

def errorTreeWithHint(using quotes: Quotes)(
    msg: String,
    hint: String,
    token: quotes.reflect.Tree
): Unit =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)
  report.error(s"$msg: $t\n  Hint: $hint", token.pos)

/** Like [[errorTreeWithHint]], but stops the macro expansion: use it where the expansion cannot
  * sensibly continue, so the user sees the actual problem and not follow-up errors.
  */
def abortTreeWithHint(using quotes: Quotes)(
    msg: String,
    hint: String,
    token: quotes.reflect.Tree
): Nothing =
  import quotes.reflect.*

  val t = token.show(using Printer.TreeStructure)
  report.errorAndAbort(s"$msg: $t\n  Hint: $hint", token.pos)

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
