package join_patterns.util

opaque type FastIterable[T] = IterableOnce[T]

extension[T] (e: FastIterable[T])
  inline def foreach[U](inline f: T => U): Unit =
    val it = e.iterator
    while it.hasNext do
      f(it.next())

extension[T] (it: IterableOnce[T])
  /**
   * Convert the Iterable into a FastIterable with an optimized inline forEach method
   * @return A FastIterable
   */
  def fast: FastIterable[T] = it
