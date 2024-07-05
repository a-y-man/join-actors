package benchmarks

import join_patterns.MatchingAlgorithm
import join_patterns.MatchingAlgorithm.BruteForceAlgorithm
import join_patterns.MatchingAlgorithm.StatefulTreeBasedAlgorithm
import joins.*
import os.group.set

import java.util.concurrent.*
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.*

import events.*

class JSizeBenchmark extends Joins:
  @volatile protected var i       = 0
  @volatile protected var endTime = 0L

  @volatile protected var maxNumberOfMatches: Option[Int] = None

  def setMaxNumberOfMatches(max: Int) =
    if maxNumberOfMatches.isEmpty then maxNumberOfMatches = Some(max)
    else throw new Exception("Max number of matches already set")

  def finalMatchCount() = i

class JSize1 extends JSizeBenchmark:
  object A extends NullaryAsyncEvent

  join { case A() =>
    i += 1
    if i == 1000 then endTime = System.currentTimeMillis()
    println(s"i = $i -- endTime = $endTime ms")
  }

class JSize2 extends JSizeBenchmark:
  object A extends NullaryAsyncEvent
  object B extends NullaryAsyncEvent

  join { case A() and1 B() =>
    i += 1
    endTime = System.currentTimeMillis()

  }

class JSize3 extends JSizeBenchmark:
  object A extends NullaryAsyncEvent
  object B extends NullaryAsyncEvent
  object C extends NullaryAsyncEvent

  join { case A() and1 B() and1 C() =>
    i += 1
    endTime = System.currentTimeMillis()

  }

class JSize4 extends JSizeBenchmark:
  object A extends NullaryAsyncEvent
  object B extends NullaryAsyncEvent
  object C extends NullaryAsyncEvent
  object D extends NullaryAsyncEvent

  join { case A() and1 B() and1 C() and1 D() =>
    i += 1
    endTime = System.currentTimeMillis()

  }

class JSize5 extends JSizeBenchmark:
  object A extends NullaryAsyncEvent
  object B extends NullaryAsyncEvent
  object C extends NullaryAsyncEvent
  object D extends NullaryAsyncEvent
  object E extends NullaryAsyncEvent

  join { case A() and1 B() and1 C() and1 D() and1 E() =>
    i += 1
    endTime = System.currentTimeMillis()

  }

class JSize6 extends JSizeBenchmark:
  object A extends NullaryAsyncEvent
  object B extends NullaryAsyncEvent
  object C extends NullaryAsyncEvent
  object D extends NullaryAsyncEvent
  object E extends NullaryAsyncEvent
  object F extends NullaryAsyncEvent

  join { case A() and1 B() and1 C() and1 D() and1 E() and1 F() =>
    i += 1
    endTime = System.currentTimeMillis()

  }

object TestJSizeBenchmark extends App:
  val benchmark = new JSize1
  benchmark.setMaxNumberOfMatches(100)
  benchmark.A()
