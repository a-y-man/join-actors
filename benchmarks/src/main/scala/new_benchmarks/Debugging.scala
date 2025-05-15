package new_benchmarks

import scala.quoted.*
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentLinkedQueue

inline val DEBUG = false

val lines = ConcurrentLinkedQueue[String]()

def writeLog(): Unit =
  if DEBUG then Files.write(Paths.get("log.txt"), lines)

private def logCodegen(expr: Expr[String])(using q: Quotes): Expr[Unit] =
  if DEBUG then '{ lines.add($expr) }
  else '{ () }

inline def log(inline s: String): Unit =
  ${ logCodegen('s) }
