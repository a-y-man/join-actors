# Fair Join Pattern Matching for Actors

## Description

This library implements join patterns in Scala 3, a coordination mechanism for concurrent message-passing programs, first introduced in the join calculus. Join patterns allow to declaratively specify how to react and synchronize distributed computations.

The library offers a convenient and safe method to define and utilize join patterns in Scala 3. It achieves this by leveraging Scala 3's metaprogramming capabilities, extending the language through the use of macros and the reflection API. We use the Scala 3 pattern matching syntax to define join patterns, and the library converts these into an internal representation using the aforementioned techniques.

Additionally, the library uses the Actor model as a practical example to demonstrate the application of join patterns. At present, we are employing a simple homemade actor model implementation.

### Code base Structure

The library is structured as follows:

- `join-patterns/core`: Contains the core implementation of the join patterns library.

  - `join-patterns/core/src/main/scala/`: Has the following sub-packages:

    - `actors`: Contains the prototype actor implementation.

    - `join_patterns`: Contains the implementation of the join pattern
      matching algorithm and code generation macros.

    - `examples`: Contains examples of using the join patterns library.

- `join-patterns/benchmarks`: Contains the benchmarking code for the join patterns library.

  - `join-patterns/benchmarks/src/main/scala/`: Has the following files:

    - `Benchmarks.scala`: Contains the benchmarking code for the join patterns library.

    - `SmartHouse.scala`: Contains the Smart House example used for benchmarking.

    - `Size.scala`: Contains the benchmarks for _Pattern size without guards_ benchmarks.

    - `SizeWithGuards.scala`: Contains the benchmarks for _Pattern size with guards_ benchmarks.

### Build and Test

The library can be compiled and run using [Scala 3.3.3](https://www.scala-lang.org) and [sbt 1.9.9](https://www.scala-sbt.org/).

To compile the library, run the following command from the root directory (where the `build.sbt` file is located):

```bash
sbt clean compile
```

To run the tests of the core library, run the following command:

```bash
sbt core/test
```

For instance, to run the Smart House benchmark, run the following command:

```bash
sbt "benchmarks/run smart-house-config -n SmartHouse --warmup-repititions 5 --repititions 5 --write-to-file -m 1000 -r 32 -s 4"
```

The above command for instance shows how to run the bounded buffer benchmark
with the specified parameters. If the `--write-to-file` flag is provided, the
results will be written to a file in the `benchmarks/data` directory. The
generated data is in CSV format and will be saved in directories named after the
benchmark name prefixed with the current date and time.
