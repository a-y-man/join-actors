package benchmarks.size_with_guards

final case class GuardedSizeConfig(
    matches: Int,
    variant: GuardedSizeVariant,
    numberOfNoiseMsgs: Option[Int] = None,
    nonMatchingPayload: Option[Int] = None
)
