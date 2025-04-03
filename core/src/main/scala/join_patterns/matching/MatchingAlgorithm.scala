package join_patterns.matching

enum MatchingAlgorithm:
  case BruteForceAlgorithm
  case StatefulTreeBasedAlgorithm
  case MutableStatefulAlgorithm
  case LazyMutableAlgorithm
  case WhileLazyAlgorithm
  case WhileEagerAlgorithm
  case EagerParallelAlgorithm(numThreads: Int)
  case LazyParallelAlgorithm(numThreads: Int)

  override def toString: String =
    this match
      case MatchingAlgorithm.BruteForceAlgorithm => "BruteForceAlgorithm"
      case MatchingAlgorithm.StatefulTreeBasedAlgorithm => "StatefulTreeBasedAlgorithm"
      case MatchingAlgorithm.MutableStatefulAlgorithm => "MutableStatefulAlgorithm"
      case MatchingAlgorithm.LazyMutableAlgorithm => "LazyMutableAlgorithm"
      case MatchingAlgorithm.WhileLazyAlgorithm => "WhileLazyAlgorithm"
      case MatchingAlgorithm.WhileEagerAlgorithm => "WhileEagerAlgorithm"
      case MatchingAlgorithm.EagerParallelAlgorithm(numThreads) => s"EagerParallelAlgorithm_$numThreads"
      case MatchingAlgorithm.LazyParallelAlgorithm(numThreads) => s"LazyParallelAlgorithm_$numThreads"

object MatchingAlgorithm:
  private val cmdStringsAndAlgorithms = List(
    "brute" -> BruteForceAlgorithm,
    "stateful" -> StatefulTreeBasedAlgorithm,
    "mutable" -> MutableStatefulAlgorithm,
    "lazy-mutable" -> LazyMutableAlgorithm,
    "while-lazy" -> WhileLazyAlgorithm,
    "while-eager" -> WhileEagerAlgorithm,
    "eager-parallel" -> EagerParallelAlgorithm(8),
    "lazy-parallel" -> LazyParallelAlgorithm(8)
  )

  private val cmdStringToAlgorithm = cmdStringsAndAlgorithms.toMap

  def parseFromCmdString(algorithmStr: String): Option[MatchingAlgorithm] =
    cmdStringToAlgorithm.get(algorithmStr)

  val CMD_STRINGS: Seq[String] = cmdStringsAndAlgorithms.map(_._1)
  val ALGORITHMS: Seq[MatchingAlgorithm] = cmdStringsAndAlgorithms.map(_._2)
