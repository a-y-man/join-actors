package join_patterns.matching

enum MatchingAlgorithm:
  case BruteForceAlgorithm
  case StatefulTreeBasedAlgorithm
  case MutableStatefulAlgorithm
  case LazyMutableAlgorithm
  case WhileLazyAlgorithm
  case FilteringWhileAlgorithm
  case WhileEagerAlgorithm
  case EagerParallelAlgorithm(numThreads: Int)
  case LazyParallelAlgorithm(numThreads: Int)
  case FilteringParallelAlgorithm(numThreads: Int)

  override def toString: String =
    this match
      case BruteForceAlgorithm => "BruteForceAlgorithm"
      case StatefulTreeBasedAlgorithm => "StatefulTreeBasedAlgorithm"
      case MutableStatefulAlgorithm => "MutableStatefulAlgorithm"
      case LazyMutableAlgorithm => "LazyMutableAlgorithm"
      case WhileLazyAlgorithm => "WhileLazyAlgorithm"
      case FilteringWhileAlgorithm => "FilteringWhileAlgorithm"
      case WhileEagerAlgorithm => "WhileEagerAlgorithm"
      case EagerParallelAlgorithm(numThreads) => s"EagerParallelAlgorithm_$numThreads"
      case LazyParallelAlgorithm(numThreads) => s"LazyParallelAlgorithm_$numThreads"
      case FilteringParallelAlgorithm(numThreads) => s"FilteringParallelAlgorithm_$numThreads"

object MatchingAlgorithm:
  private val cmdStringsAndAlgorithms = List(
    "brute" -> BruteForceAlgorithm,
    "stateful" -> StatefulTreeBasedAlgorithm,
    "mutable" -> MutableStatefulAlgorithm,
    "lazy-mutable" -> LazyMutableAlgorithm,
    "while-lazy" -> WhileLazyAlgorithm,
    "filtering-while" -> FilteringWhileAlgorithm,
    "while-eager" -> WhileEagerAlgorithm,
    "eager-parallel" -> EagerParallelAlgorithm(8),
    "lazy-parallel" -> LazyParallelAlgorithm(8),
    "filtering-parallel" -> FilteringParallelAlgorithm(8)
  )

  private val cmdStringToAlgorithm = cmdStringsAndAlgorithms.toMap

  def parseFromCmdString(algorithmStr: String): Option[MatchingAlgorithm] =
    cmdStringToAlgorithm.get(algorithmStr)

  val CMD_STRINGS: Seq[String] = cmdStringsAndAlgorithms.map(_._1)
  val ALGORITHMS: Seq[MatchingAlgorithm] = cmdStringsAndAlgorithms.map(_._2)
