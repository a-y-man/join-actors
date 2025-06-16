package new_benchmarks

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

def intercalateCorrectMsgs[A](
                                       correctMsgs: Vector[A],
                                       randomMsgs: Vector[A]
                                     ): Vector[A] =
  val randomMsgsSize = randomMsgs.size
  val correctMsgsSize = correctMsgs.size
  if randomMsgsSize >= correctMsgsSize then
    val groupSize = (randomMsgsSize + correctMsgsSize - 1) / correctMsgsSize
    randomMsgs
      .grouped(groupSize) // Chunk the random messages into chunks of size groupSize
      .zipAll(correctMsgs, randomMsgs, randomMsgs.headOption.getOrElse(correctMsgs.last))
      .flatMap { case (randomChunk, correctMsg) => randomChunk :+ correctMsg }
      .toVector
  else randomMsgs ++ correctMsgs

def genNMatchingMsgSeqs[A](patSize: Int)(generator: Int => Seq[A])(nMatches: Int) =
  Vector.fill(nMatches)(generator(patSize)).flatten
