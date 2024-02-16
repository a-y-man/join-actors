package join_patterns

import actor.ActorRef
import com.typesafe.scalalogging.*

import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.Console
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutMap

type RHSFnClosure[M, T] = (LookupEnv, ActorRef[M]) => T

type MatchIdxs = (MessageIdxs, PatternIdx)

type CandidateMatch[M, T] = Option[(MatchIdxs, (LookupEnv, RHSFnClosure[M, T]))]

type CandidateMatches[M, T] =
  TreeMap[MatchIdxs, (LookupEnv, RHSFnClosure[M, T])]

object CandidateMatches extends LazyLogging:
  import math.Ordering.Implicits.infixOrderingOps
  import math.Ordering.Implicits.seqOrdering
  def apply[M, T](): CandidateMatches[M, T] =
    TreeMap[MatchIdxs, (LookupEnv, RHSFnClosure[M, T])]()(
      Ordering.Tuple2[MessageIdxs, PatternIdx]
    )

  def logCandidateMatches[M, T](candidateMatches: CandidateMatches[M, T]) =
    val stringifiedMatches =
      candidateMatches
        .map { case ((msgIdxs, patIdx), (substs, _)) =>
          val ppMsgIdxs =
            s"${Console.UNDERLINED}${Console.GREEN}I: ${msgIdxs.mkString("[", ", ", "]")}${Console.RESET}"
          val ppPatIdx =
            s"${Console.UNDERLINED}${Console.RED}Pattern Idx: ${patIdx}${Console.RESET}"
          val ppSubsts =
            s"${Console.UNDERLINED}${Console.BLUE}Substs:${Console.RESET} ${ppLookupEnv(substs)}${Console.RESET}"
          s"${ppMsgIdxs} -- ${ppPatIdx} -- ${ppSubsts}"
        }
        .mkString("\n")

    println {
      s"\n===================CandidateMatches===================\n" +
        s"${stringifiedMatches}" +
        s"\n======================================================\n"
    }

trait Matcher[M, T] extends LazyLogging:
  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T

  def mapIdxsToFits(
      msgIdxsQ: MessageIdxs,
      patternInfo: PatternFits[M],
      messages: ListBuffer[M]
  ) =
    msgIdxsQ.foldLeft(Map[Int, PatternFits[M]]()) { (msgIdxsToFits, msgIdx) =>
      val m = messages(msgIdx)
      val msgIdxToFits = patternInfo.filter { info =>
        val ((checkMsgType, _), _) = info
        checkMsgType(m)
      }
      msgIdxsToFits.updated(msgIdx, msgIdxToFits)
    }

  def computeValidPermutations(
      msgIdxs: MessageIdxs,
      msgIdxToFits: Map[Int, PatternFits[M]]
  ): Iterator[List[(Int, M => LookupEnv)]] =
    def isInPattern(msgIdx: Int, msgsInPat: Set[Int]): Boolean =
      msgsInPat.contains(msgIdx)

    def isValidPermutation(permutation: MessageIdxs): Boolean =
      permutation.zipWithIndex.forall { (msgIdx, permIdx) =>
        val patIdxs = msgIdxToFits(msgIdx).map(_._2)
        // [3 -> [0, 2], 4 -> [1], 5 -> [0, 2]]

        // P  [0, 1, 2]
        // M  [3, 4, 5] || [5, 4, 3]
        isInPattern(permIdx, patIdxs)
      }

    def allPermutations = msgIdxs.permutations
    val validPermutations =
      allPermutations.collect {
        case permutation if isValidPermutation(permutation) =>
          permutation.map { msgIdx =>
            val possibleFits = msgIdxToFits(msgIdx)
            val ((_, extractField), _) =
              possibleFits.find(pat => pat._2 == permutation.indexOf(msgIdx)).get
            (msgIdx, extractField)
          }
      }
    validPermutations

  def computeSubsts(
      messages: ListBuffer[M],
      possibleFit: List[(Int, M => LookupEnv)]
  ) =
    possibleFit.foldLeft(LookupEnv.empty) { (substsAcc, msgData) =>
      val (msgIdx, extractField) = msgData
      val subs                   = extractField(messages(msgIdx))
      substsAcc ++ subs
    }

  def findBestMatch(
      validPermutations: Iterator[List[(Int, M => LookupEnv)]],
      messages: ListBuffer[M],
      pattern: JoinPattern[M, T]
  ) =
    var bestMatchSubsts: LookupEnv = null
    var bestMatchIdxs: MessageIdxs = null
    validPermutations.find { possibleFit =>
      bestMatchSubsts = computeSubsts(messages, possibleFit)
      // logger.debug(s"Possible fit: ${possibleFit.map(_._1).mkString(", ")}\n")
      if pattern.guard(bestMatchSubsts) then
        bestMatchIdxs = possibleFit.map(_._1)
        true
      else false
    }
    if bestMatchIdxs != null && bestMatchSubsts != null then Some((bestMatchIdxs, bestMatchSubsts))
    else None

  // remove all messages from the queue that have been processed
  def removeProcessedMsgs(messages: ListBuffer[(M, Int)], processedMsgs: MessageIdxs) =
    messages.filterNot((_, idx) => processedMsgs.contains(idx))

object SelectMatcher:
  def apply[M, T](algorithm: MatchingAlgorithm, patterns: List[JoinPattern[M, T]]): Matcher[M, T] =
    algorithm match
      case MatchingAlgorithm.BruteForceAlgorithm        => BruteForceMatcher(patterns)
      case MatchingAlgorithm.StatefulTreeBasedAlgorithm => StatefulTreeMatcher(patterns)

class BruteForceMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[M]()
  private val patternsWithIdxs = patterns.zipWithIndex

  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    import scala.jdk.CollectionConverters.*

    var result: Option[T] = None

    if messages.isEmpty then
      messages.append(q.take())
      // logger.debug(s"Queue: ${messages.mkString(", ")}")

    while result.isEmpty do
      val indexedMessages = messages.zipWithIndex
      val candidateMatches: CandidateMatches[M, T] =
        patternsWithIdxs.foldLeft(CandidateMatches[M, T]()) {
          (candidateMatchesAcc, patternWithIdx) =>
            val (pattern, patternIdx) = patternWithIdx
            if messages.size >= pattern.size then
              val possibleMatches = pattern.extract(messages.toList)
              possibleMatches match
                case Some((candidateIdxsQ, patternInfo)) =>
                  val bestMatch = candidateIdxsQ
                    .map { candidateI =>
                      val msgIdxsToFits =
                        mapIdxsToFits(candidateI, patternInfo, messages)

                      val validPermutations = computeValidPermutations(candidateI, msgIdxsToFits)
                      findBestMatch(validPermutations, messages, pattern)
                    }
                    .collectFirst { case Some(_bestMatch) => _bestMatch }

                  bestMatch match
                    case Some((bestMatchIdxs, bestMatchSubsts)) =>
                      // println(s"bestMatchIdxs: $bestMatchIdxs -- bestMatchSubsts: $bestMatchSubsts")
                      val selectedMatch =
                        (
                          bestMatchSubsts,
                          (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
                        )
                      // println(s"Selected match: $selectedMatch")
                      candidateMatchesAcc.updated((bestMatchIdxs, patternIdx), selectedMatch)
                    case None => candidateMatchesAcc

                case None => candidateMatchesAcc
            else candidateMatchesAcc
        }
      if candidateMatches.nonEmpty then
        // CandidateMatches.logCandidateMatches(candidateMatches)
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head

        result = Some(rhsFn(substs, selfRef))

        val unprocessedMsgs = removeProcessedMsgs(indexedMessages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs.map(_._1))

      if result.isEmpty then messages.append(q.take())

    result.get

class StatefulTreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]])
    extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  val messages                 = ListBuffer[(M, Int)]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls
  var initMatchingTree = MatchingTree()
  val initPatternBins  = PatternBins()

  var patternsWithMatchingTrees: List[PatternState[M, T]] = patternsWithIdxs
    .map { patternsWithIdxs =>
      (patternsWithIdxs, (initMatchingTree, initPatternBins))
    }

  def findMatch(
      newMsg: (M, Int),
      patternState: PatternState[M, T]
  ): (PatternState[M, T], CandidateMatch[M, T]) =

    val (mQ, mQidx)                             = newMsg
    val ((pattern, patternIdx), (mTree, pBins)) = patternState
    val updatedMatchingTree = pattern.partialExtract((mQ, mQidx), (mTree, PatternExtractors()))
    val messages_           = messages.map(_._1).toList

    updatedMatchingTree match
      case Some((updatedMTree, patExtractors)) =>
        // logMapping(ppTree(updatedMTree), s" Matching Tree for JP ${patternIdx} ")
        // logMapping(ppPatternExtractors(patExtractors), s" Pattern Extractors for JP ${patternIdx} ")
        val completePatterns = findCompletePatterns(updatedMTree, pattern.size)

        // logger.info(
        //   s"Complete Patterns: ${completePatterns.map((k, v) => (k, v)).mkString("\n")}"
        // )

        val possibleMatches = completePatterns.iterator
          .map { (msgIdxs, patternBins) =>

            val validPermutations =
              computeValidPermutations_[M, T](msgIdxs, patExtractors, patternBins)

            val bestMatchOpt = findBestMatch(validPermutations, messages.map(_._1), pattern)

            // Some((msgIdxs, matchOpt))
            bestMatchOpt
          }
          .collectFirst { case Some(_bestMatch) => _bestMatch }

        possibleMatches match
          case Some((bestMatchIdxs, bestMatchSubsts)) =>
            val selectedMatch =
              (
                bestMatchSubsts,
                (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
              )

            val removedNonMatchingNodes =
              updatedMTree.removedAll(completePatterns.removed(bestMatchIdxs.sorted).keySet)
            // logger.info(
            //   s"Some(${bestMatchIdxs}) -- Removed non-matching nodes: \n${ppTree(removedNonMatchingNodes)}"
            // )
            (
              ((pattern, patternIdx), (removedNonMatchingNodes, pBins)),
              Some((bestMatchIdxs, patternIdx), selectedMatch)
            )
          case None =>
            val removedNonMatchingNodes = updatedMTree.removedAll(completePatterns.keySet)
            // logger.info(
            //   s"None -- Removed non-matching nodes: \n${ppTree(removedNonMatchingNodes)}"
            // )
            (((pattern, patternIdx), (removedNonMatchingNodes, pBins)), None)

      case None => (patternState, None)

  def collectCandidateMatches(
      newMsg: (M, Int),
      patternStates: List[PatternState[M, T]]
  ): List[(PatternState[M, T], CandidateMatch[M, T])] =
    patternStates flatMap { patternState =>
      List(findMatch(newMsg, patternState))
    }

  var mQidx = -1
  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    import scala.jdk.CollectionConverters.*

    var result: Option[T] = None
    var mQ                = q.take()
    mQidx += 1
    messages.append((mQ, mQidx))
    // logger.debug(
    //   s"Queue: \n${messages.map((m, i) => (m.getClass().getSimpleName(), i)).mkString("\n")}"
    // )

    while result.isEmpty do
      val (updatedPatternStates, possibleMatches) =
        collectCandidateMatches((mQ, mQidx), patternsWithMatchingTrees).unzip

      val candidateMatches: CandidateMatches[M, T] =
        possibleMatches.foldLeft(CandidateMatches[M, T]()) {
          case (acc, Some(candidateMatch)) =>
            val (msgIdxs, p) = candidateMatch
            acc.updated(msgIdxs, p)
          case (acc, None) => acc
        }

      patternsWithMatchingTrees = updatedPatternStates
      // val ppTrees = updatedPatternStates map { (pState: PatternState[M, T]) =>
      //   val (joinPat, currentMTree) = pState
      //   currentMTree.ppTree
      // }

      // logger.debug(ppTrees.mkString("\n"))

      if candidateMatches.nonEmpty then
        // CandidateMatches.logCandidateMatches(candidateMatches)
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs, selfRef))

        // logger.info(s"Result: $result")
        // Prune tree
        patternsWithMatchingTrees = patternsWithMatchingTrees.map {
          case (joinPat, (currentMTree, pBins)) =>
            val prunedTree = pruneTree(currentMTree, candidateQidxs)
            (joinPat, (prunedTree, pBins))
        }

        // patternsWithMatchingTrees foreach { (joinPat, prunedTree) =>
        //   logMapping(
        //     ppTree(prunedTree._1),
        //     s" Matching Tree for JP ${joinPat._2} after pruning ${candidateQidxs.mkString("{", ", ", "}")} "
        //   )
        // }

      if result.isEmpty then
        mQ = q.take()
        mQidx += 1
        messages.append((mQ, mQidx))
      // logger.info(
      //   s"Queue: \n${messages.map((m, i) => (m.getClass().getSimpleName(), i)).mkString("\n")}"
      // )
    result.get
