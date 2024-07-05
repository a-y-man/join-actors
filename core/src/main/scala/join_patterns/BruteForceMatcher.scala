package join_patterns

import actor.ActorRef

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.ArrayBuffer

def logMailBoxSizeAndMsgCnt[M](mBox: ArrayBuffer[M], msgCount: Int): String =
  s"$msgCount,${mBox.size}"

class BruteForceMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ArrayBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // val filename0 = "brute_force_matcher_size_0_random_msgs_3_valid_msgs.csv"
  // val filename3 = "brute_force_matcher_size_3_random_msgs_3_valid_msgs.csv"
  // val filename6 = "brute_force_matcher_size_6_random_msgs_3_valid_msgs.csv"

  // val logs: ArrayBuffer[String] = ArrayBuffer()
  // logs.append("Message Count,Mailbox Size")
  // logs.append("0,0")

  // appendToFile(filename6, logs.head + "\n" + "0,0\n")

  private var msgCounter = 0

  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    if messages.isEmpty then messages.append(q.take())
    // msgCounter += 1

    // val currentLog = logMailBoxSizeAndMsgCnt(messages, msgCounter)
    // logs.append(currentLog)
    // appendToFile(filename6, currentLog + "\n")
    // println(s"BruteForceMatcher Logs:\n${logs.mkString("\n")}")

    while result.isEmpty do
      val indexedMessages = messages.zipWithIndex
      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            if messages.size >= pattern.size then
              val patternBinsOpt = pattern.extract(messages.toList)
              patternBinsOpt match
                case Some(patternBins) =>
                  val validPermutations =
                    getMsgIdxsWithPayloadExtractor(
                      pattern.getPatternInfo.patternExtractors,
                      patternBins
                    )

                  val bestMatchOpt = findFairestMatch(validPermutations, messages, pattern)

                  bestMatchOpt match
                    case Some((bestMatchIdxs, bestMatchSubsts)) =>
                      val selectedMatch =
                        (
                          bestMatchSubsts,
                          (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
                        )
                      candidateMatchesAcc.updated((bestMatchIdxs, patternIdx), selectedMatch)
                    case None => candidateMatchesAcc

                case None => candidateMatchesAcc
            else candidateMatchesAcc
        }
      if candidateMatches.nonEmpty then
        val ((candidateQidxs, _), (substs, rhsFn)) = candidateMatches.head

        result = Some(rhsFn(substs, selfRef))

        val unprocessedMsgs = removeProcessedMsgs(indexedMessages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs.map(_._1))

      // println(s"BruteForceMatcher Messages: ${messages.mkString(", ")}")

      if result.isEmpty then
        messages.append(q.take())
        // msgCounter += 1

        // val currentLog = logMailBoxSizeAndMsgCnt(messages, msgCounter)
        // logs.append(currentLog)
        // appendToFile(filename6, currentLog + "\n")
        // println(s"BruteForceMatcher Logs:\n${logs.mkString("\n")}")

    result.get
