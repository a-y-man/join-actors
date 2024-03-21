package join_patterns

import actor.ActorRef
import com.typesafe.scalalogging.*

import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.Console
import scala.collection.immutable.Iterable
import scala.collection.immutable.Queue
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

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

  def crossProduct[T](
      list: LazyList[LazyList[T]]
  ): LazyList[LazyList[T]] =
    /** The following `crossProduct` method is adapted from the following Stack Overflow answer:
      * https://stackoverflow.com/a/54333046/
      */
    list match
      case LazyList()       => LazyList()
      case x #:: LazyList() => x.map(LazyList(_))
      case x #:: xs =>
        val tail = crossProduct(xs)
        for
          i <- x
          j <- tail
        yield LazyList(i) #::: j

  def computeValidCombinations(
      patternBins: PatternBins
  ) =
    val combs = patternBins.view
      .flatMap((patternShape, messageIdxs) => // [ { 0, 2 } -> { 3, 5 }, { 1 } -> { 4 } ]
        val msgsPermutation = messageIdxs // [ 3, 5 ] or [ 4 ]
          .combinations(patternShape.size) // [ 3, 5 ] or [ 4 ]
          .map(_.permutations)             // [[ 3, 5 ], [ 5, 3 ]] or [[ 4 ]]
          .map(l =>
            l.map(patternShape zip _).to(LazyList)
          ) // [[ (0, 3), (2, 5) ], [ (0, 5), (2, 3) ]] or [[ (1, 4) ]]
        TreeMap(patternShape -> msgsPermutation.to(LazyList))(
          patternIdxOrdering
        ) // { [ 0, 2 ] -> [[ (0, 3), (2, 5) ], [ (0, 5), (2, 3) ]] } or { [ 1 ] -> [[ (1, 4) ]] }
      )
    // combs.values = [[[ (0, 3), (2, 5) ], [ (0, 5), (2, 3) ]], [[ (1, 4) ]]]
    // crossProduct = [[ (0, 3), (2, 5), (1, 4) ], [ (0, 5), (2, 3), (1, 4) ]]
    // println(combs.map(_._2).to(List).flatten.map(_.toList))
    crossProduct(
      combs.map(_._2).to(LazyList)
    ).flatMap(crossProduct(_)).map(_.flatten)

  def findValidPermutations[M, T](
      patExtractors: PatternExtractors[M],
      patternBins: PatternBins
  ): Iterator[List[(Int, M => Map[String, Any])]] =
    val validCombinations = computeValidCombinations(patternBins).iterator
    for
      combination <- validCombinations
      validCombination = combination.sortBy(_._1)
    yield validCombination.map { case (pidx, msgIdx) =>
      val (_, extractField) = patExtractors(pidx)
      (msgIdx, extractField)
    }.toList

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
      // println(s"Possible fit: ${possibleFit.map(_._1).mkString(", ")}\n")
      if pattern.guard(bestMatchSubsts) then
        bestMatchIdxs = MessageIdxs(possibleFit.map(_._1)*)
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
              val patternBinsOpt = pattern.extract(messages.toList)
              patternBinsOpt match
                case Some(patternBins) =>
                  val validPermutations =
                    findValidPermutations[M, T](
                      pattern.getPatternInfo.patternExtractors,
                      patternBins
                    )

                  val bestMatchOpt = findBestMatch(validPermutations, messages, pattern)

                  bestMatchOpt match
                    case Some((bestMatchIdxs, bestMatchSubsts)) =>
                      // println(
                      //   s"bestMatchIdxs: $bestMatchIdxs -- bestMatchSubsts: ${ppLookupEnv(bestMatchSubsts)}"
                      // )
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
        val ((candidateQidxs, _), (substs, rhsFn)) = candidateMatches.head

        result = Some(rhsFn(substs, selfRef))

        val unprocessedMsgs = removeProcessedMsgs(indexedMessages, candidateQidxs)
        messages.clear()
        messages.addAll(unprocessedMsgs.map(_._1))

      if result.isEmpty then messages.append(q.take())

    result.get

class StatefulTreeMatcher[M, T](private val patterns: List[JoinPattern[M, T]])
    extends Matcher[M, T]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  private val messages         = ListBuffer[(M, Int)]()
  private val patternsWithIdxs = patterns.zipWithIndex

  // Init patterns with empty MatchingTree and maintain across apply() calls

  private var patternsWithMatchingTrees: List[PatternState[M, T]] = patternsWithIdxs
    .map { case p @ (pattern, _) =>
      val patInfo                          = pattern.getPatternInfo
      val (initPatternBins, patExtractors) = (patInfo._1, patInfo._2)

      val initMatchingTree = MatchingTree().updated(MessageIdxs(), initPatternBins)
      (p, (initMatchingTree, patInfo))
    }

  private def findMatch(
      newMsg: (M, Int),
      patternState: PatternState[M, T]
  ): (PatternState[M, T], CandidateMatch[M, T]) =

    val (mQ, mQidx)                               = newMsg
    val ((pattern, patternIdx), (mTree, patInfo)) = patternState
    val updatedMatchingTree                       = pattern.partialExtract((mQ, mQidx), mTree)
    val messages_                                 = messages.map(_._1).toList

    updatedMatchingTree match
      case Some(updatedMTree) =>
        // logMapping(ppTree(updatedMTree), s" Matching Tree for JP ${patternIdx} ")
        // logMapping(ppPatternExtractors(patExtractors), s" Pattern Extractors for JP ${patternIdx} ")
        val completePatterns = findCompletePatterns(updatedMTree, pattern.size)

        // println(
        //   s"Complete Patterns:\n${completePatterns.map((k, v) => (k, v)).mkString("\n")}"
        // )

        val possibleMatches = completePatterns.iterator
          .map { (msgIdxs, patternBins) =>
            val validPermutations =
              findValidPermutations[M, T](patInfo.patternExtractors, patternBins)
            val bestMatchOpt = findBestMatch(validPermutations, messages.map(_._1), pattern)
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
              updatedMTree.removedAll(completePatterns.removed(bestMatchIdxs).keySet)
            // logger.info(
            //   s"Some(${bestMatchIdxs}) -- Removed non-matching nodes: \n${ppTree(removedNonMatchingNodes)}"
            // )
            (
              ((pattern, patternIdx), (removedNonMatchingNodes, patInfo)),
              Some((bestMatchIdxs, patternIdx), selectedMatch)
            )
          case None =>
            val removedNonMatchingNodes = updatedMTree.removedAll(completePatterns.keySet)
            // logger.info(
            //   s"None -- Removed non-matching nodes: \n${ppTree(removedNonMatchingNodes)}"
            // )
            (((pattern, patternIdx), (removedNonMatchingNodes, patInfo)), None)

      case None => (patternState, None)

  private def collectCandidateMatches(
      newMsg: (M, Int),
      patternStates: List[PatternState[M, T]]
  ): List[(PatternState[M, T], CandidateMatch[M, T])] =
    patternStates flatMap { patternState =>
      List(findMatch(newMsg, patternState))
    }

  private var mQidx = -1
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

      patternsWithMatchingTrees = updatedPatternStates

      val candidateMatches: CandidateMatches[M, T] =
        possibleMatches.foldLeft(CandidateMatches[M, T]()) {
          case (acc, Some(candidateMatch)) =>
            val (msgIdxs, p) = candidateMatch
            acc.updated(msgIdxs, p)
          case (acc, None) => acc
        }

      if candidateMatches.nonEmpty then
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs, selfRef))

        // Prune tree
        patternsWithMatchingTrees = patternsWithMatchingTrees.map {
          case (joinPat, (currentMTree, pBins)) =>
            val prunedTree = pruneTree(currentMTree, candidateQidxs)
            (joinPat, (prunedTree, pBins))
        }

      if result.isEmpty then
        mQ = q.take()
        mQidx += 1
        messages.append((mQ, mQidx))

    result.get
