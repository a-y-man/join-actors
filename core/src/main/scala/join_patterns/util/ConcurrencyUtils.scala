package join_patterns.util

import java.util.Spliterator
import scala.collection.mutable.ArrayDeque

def divideSpliterator[E](s: Spliterator[E], num: Int): ArrayDeque[Spliterator[E]] =
  val q = ArrayDeque(s)
  var exhausted = false
  while q.length < num && !exhausted do
    val spliterator = q.removeHead(false)
    val newSpliterator = spliterator.trySplit()

    if newSpliterator == null then
      q.append(spliterator)
      exhausted = true
    else
      q.append(spliterator)
      q.append(newSpliterator)
  q