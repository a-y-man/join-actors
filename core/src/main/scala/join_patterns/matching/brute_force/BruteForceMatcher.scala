package join_patterns.matching.brute_force

import join_actors.actor.ActorRef
import join_patterns.matching.functions.*
import join_patterns.matching.*
import join_patterns.types.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.mutable.Map as MutMap

class BruteForceMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = MutMap[Int, M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  private var mQidx = -1

  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    var mQ = q.take()
    mQidx += 1
    messages.update(mQidx, mQ)

    while result.isEmpty do
      // val indexedMessages = messages.zipWithIndex
      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            if messages.size >= pattern.size then
              val patternBinsOpt = pattern.extract(messages.toMap)
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

        // Remove selected message indices from messages
        candidateQidxs.foreach { idx =>
          messages.remove(idx)
        }

      if result.isEmpty then
        mQ = q.take()
        mQidx += 1
        messages.update(mQidx, mQ)

    result.get
