package join_patterns

import actor.ActorRef
import com.typesafe.scalalogging.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.ArrayBuffer

class BruteForceMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ArrayBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    if messages.isEmpty then messages.append(q.take())

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

      if result.isEmpty then messages.append(q.take())

    result.get
