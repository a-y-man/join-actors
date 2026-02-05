# JoinActors: Fair Join Pattern Matching for Actors

## Description

This library implements join patterns in Scala 3, a coordination mechanism for
concurrent message-passing programs, first introduced in the join calculus. Join
patterns allow to declaratively specify how to react and synchronize distributed
computations.

The library offers a convenient and safe method to define and utilize join
patterns in Scala 3. It achieves this by leveraging Scala 3's metaprogramming
capabilities, extending the language through the use of macros and the
reflection API. We use the Scala 3 pattern matching syntax to define join
patterns, and the library converts these into an internal representation using
the aforementioned techniques.

Additionally, the library uses the Actor model as a practical example to
demonstrate the application of join patterns. At present, we are employing a
simple homemade actor model implementation. 

The formal foundation upon which this library is based can be found in the
[ECOOP'24 paper](https://doi.org/10.4230/LIPIcs.ECOOP.2024.17). A comprehensive description of the implementation of the
library can be found in the [Programming Journal 25 paper](https://doi.org/10.22152/programming-journal.org/2026/11/4).

## Code base structure

- [`core/`](core) - Core join patterns library implementation
  - [`core/src/main/scala/join_patterns/`](core/src/main/scala/join_patterns) - Implementation of join patterns
  - [`core/src/main/scala/examples/`](core/src/main/scala/examples) - Usage examples
- [`benchmarks/`](benchmarks) - Performance benchmarking suite

## API usage

The library provides a simple API to define join patterns. The following example
demonstrates how to define a join pattern for a simple factory shop floor
monitoring system, as seen in the paper:

- First add the necessary imports:

```scala
import join_actors.api.*
```

- Define the messages that the actors will exchange. We use Scala 3 enums to
  define the message types with their respective payloads:

```scala
enum MachineEvent:
  case Fault(faultID: Int, ts: Long)

enum WorkerEvent:
  case Fix(faultID: Int, ts: Long)

enum SystemEvent:
  case DelayedFault(faultID: Int, ts: Long)
  case Shutdown()

type Event = MachineEvent | WorkerEvent | SystemEvent
```

- Now we define the monitor as an actor with join definitions using the `receive` macro.
  Join patterns are written as message patterns separated by the `&:&` operator, and followed by a guard condition. 
  The old tuple-like syntax is also supported

```scala
def monitor(matcher: MatcherFactory) =
  Actor[Event, Unit] {
    receive { (self: ActorRef[Event]) =>
      {
        case Fault(fid1, ts1) &:& Fix(fid2, ts2) if fid1 == fid2 => ...
        case Fault(fid1, ts1) &:& Fault(fid2, ts2) &:& Fix(fid3, ts3)
            if fid2 == fid3 && ts2 > ts1 + TEN_MIN => ...
        case DelayedFault(fid1, ts1) &:& Fix(fid2, ts2) if fid1 == fid2 => ...
        ...
      }
    }(matcher)
  }
```

- Finally, we can run the monitor as follows:

```scala
def runFactorySimple(matcher: MatcherFactory) =
  // Predefined sequence of events
  val events = List(
    Fault(1, ONE_MIN),
    Fault(2, TEN_MIN),
    Fault(3, QUARTER_HR),
    Fix(3, THIRTY_MIN)
  )

  // Start the monitor actor
  val (monitorFut, monitorRef) = monitor(matcher).start()

  // Send the events to the monitor actor
  events foreach (event => monitorRef ! event)

  // Shutdown the monitor actor
  monitorRef ! Shutdown()

  // Wait for the monitor actor to finish
  Await.ready(monitorFut, Duration(15, "m"))
```

The matcher can be set to any `MatcherFactory` implementations, allowing access to all matchers.
In the example above, some minor details are omitted for brevity. The full
example can be found in the [FactorySimpl.scala](core/src/main/scala/examples/FactorySimpl.scala) file.


## Build and test

The library can be compiled by installing a [Java Development Kit (version 21 or later)](https://jdk.java.net/21/) and
[sbt (version 1.9 or later)](https://www.scala-sbt.org/) and running `sbt compile`. Then, `sbt` will download the required
dependencies (including the Scala 3 compiler).

### Using sbt directly

To compile the library, run the following command from the root directory (where
the `build.sbt` file is located):

```bash
sbt clean compile
```

To run the tests of the core library, run the following command:

```bash
sbt core/test
```

To run for instance the `Factory Simple` example with the predefined configuration
run the following command:

```bash
sbt "core/run factory-simple --matcher stateful"
```

### Using the Makefile

A `Makefile` is provided for convenient access to common tasks. Run `make help` to see all available targets:

```bash
make help
```

### Available matchers

The `--matcher` flag can be set to any of the following, allowing access to all implemented matchers:

- [`brute`](core/src/main/scala/join_patterns/matching/brute_force)
- [`stateful`](core/src/main/scala/join_patterns/matching/immutable)
- [`mutable`](core/src/main/scala/join_patterns/matching/mutable)
- [`lazy-mutable`](core/src/main/scala/join_patterns/matching/lazy_mutable)
- [`while-lazy`](core/src/main/scala/join_patterns/matching/while_lazy)
- [`while-eager`](core/src/main/scala/join_patterns/matching/while_eager)
- [`eager-parallel`](core/src/main/scala/join_patterns/matching/eager_parallel)
- [`lazy-parallel`](core/src/main/scala/join_patterns/matching/lazy_parallel)
- [`filtering-while`](core/src/main/scala/join_patterns/matching/filtering_while)
- [`filtering-parallel`](core/src/main/scala/join_patterns/matching/filtering_parallel)
- [`array-while`](core/src/main/scala/join_patterns/matching/array_while)

There are other examples available in the [`examples`](core/src/main/scala/examples) package that can be run in a similar way.

## Benchmarks

See the [benchmarks README](benchmarks/README.md) for more information on how to run the benchmarks.

## References

- Philipp Haller, Ayman Hussein, Hernán Melgratti, Alceste Scalas, and Emilio Tuosto. Fair Join Pattern Matching for Actors. In 38th European Conference on Object-Oriented Programming (ECOOP 2024). Leibniz International Proceedings in Informatics (LIPIcs), Volume 313, pp. 17:1-17:28, Schloss Dagstuhl – Leibniz-Zentrum für Informatik (2024) https://doi.org/10.4230/LIPIcs.ECOOP.2024.17

- Ayman Hussein, Philipp Haller, Ioannis Karras, Hernán Melgratti, Alceste Scalas, and Emilio Tuosto. ‘JoinActors: A Modular Library for Actors with Join Patterns’. The Art, Science, and Engineering of Programming. 15 February 2026. https://doi.org/10.22152/programming-journal.org/2026/11/4