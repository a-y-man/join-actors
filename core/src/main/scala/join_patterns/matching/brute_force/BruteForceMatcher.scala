package join_patterns.matching.brute_force

import join_actors.actor.ActorRef
import join_patterns.matching.functions.*
import join_patterns.matching.*
import join_patterns.types.*

import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.collection.compat.immutable.ArraySeq
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
              val patternBinsOpt = extractPatternBins(pattern, messages.toMap)
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

  private def extractPatternBins(pattern: JoinPattern[M, T], messages: Messages[M]): Option[PatternBins] =
    val msgPatterns = pattern.getPatternInfo.patternExtractors

    val messageIdxWithFits = getMsgIdxsWithFits(messages, msgPatterns)
    val initPatternBins = pattern.getPatternInfo.patternBins
    val patternBins = buildPatternBins(messageIdxWithFits, initPatternBins)

    if !isPatternBinComplete(patternBins) then None
    else Some(patternBins)

  private def getMsgIdxsWithFits(
                          messages: Map[Int, M],
                          msgPatterns: PatternExtractors[M]
                        ): Map[MessageIdx, PatternIdxs] =
    messages.iterator
      // Associate each message index with the list of pattern indices that match it
      .map { case (idx, msg) =>
        val matches =
          msgPatterns.filter { case (_idx, PatternIdxInfo(checkMsgType, _fieldExtractor, _)) => checkMsgType(msg) }.keys.to(PatternIdxs)
        (idx, matches)
      }
      // Take only the results with at least one matching pattern
      .filter(_._2.nonEmpty)
      .toMap

  private def buildPatternBins(
                        messageIdxWithFits: Map[MessageIdx, PatternIdxs],
                        initialPatternBins: PatternBins
                      ): PatternBins =
    messageIdxWithFits.foldLeft(initialPatternBins) { case (acc, (messageIdx, patternShape)) =>
      acc.updatedWith(patternShape) {
        case Some(messageIdxs) =>
          if messageIdxs.contains(messageIdx) then Some(messageIdxs)
          else Some(messageIdxs :+ messageIdx)
        case None => Some(MessageIdxs())
      }
    }

  private def isPatternBinComplete(
                            patternBins: PatternBins
                          ): Boolean =
    patternBins.forall((patShape, msgIdxs) => msgIdxs.size >= patShape.size)
