package examples.payment

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

extension[A: ClassTag](seq1: ArraySeq[A])
  def intercalate(seq2: ArraySeq[A]): ArraySeq[A] =
    val intercalated = seq1.zip(seq2).flatMap((a, b) => ArraySeq(a, b))

    val length1 = seq1.length
    val length2 = seq2.length
    val remaining =
      if length1 > length2 then seq1.takeRight(length1 - length2)
      else if length2 > length1 then seq2.takeRight(length2 - length1)
      else ArraySeq()

    intercalated ++ remaining