package test.utils

import join_actors.api.*
import org.scalatest.prop.TableDrivenPropertyChecks.Table

val matchers = Table(
  "Matcher",
  ArrayParallelMatcher(2),
  ArrayWhileMatcher,
  BruteForceMatcher,
  EagerParallelMatcher(2),
  FilteringParallelMatcher(2),
  FilteringWhileMatcher,
  LazyMutableMatcher,
  LazyParallelMatcher(2),
  MutableStatefulMatcher,
  StatefulTreeMatcher,
  WhileEagerMatcher,
  WhileLazyMatcher
)
