package benchmarks.size

final case class SizeConfig (
    matches: Int,
    noise: Boolean,
    numberOfNoiseMsgs: Int = 0
)
