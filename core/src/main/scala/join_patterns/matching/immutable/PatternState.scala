package join_patterns.matching.immutable

import join_patterns.types.{JoinPattern, PatternInfo}

/**
 * A tuple ((pattern, patternIdx), (mTree, patInfo))
 *
 * @tparam M The message type
 * @tparam T The result type
 */
type PatternState[M, T] = ((JoinPattern[M, T], Int), (MatchingTree, PatternInfo[M]))