# Join Patterns in Scala

Author: [Ayman H.](https://github.com/a-y-man)

## Description

This library implements join patterns in Scala 3, a coordination mechanism for concurrent message-passing programs, first introduced in the join calculus. Join patterns allow to declaratively specify how to react and synchronize distributed computations.

The library offers a convenient and safe method to define and utilize join patterns in Scala 3. It achieves this by leveraging Scala 3's metaprogramming capabilities, extending the language through the use of macros and the reflection API. We use the Scala 3 pattern matching syntax to define join patterns, and the library these into an internal representation using the aforementioned techniques.

Additionally, the library uses the Actor model as a practical example to demonstrate the application of join patterns. At present, we are employing a simple homemade actor model implementation.

### Build and Test

The library can be compiled and run using [Scala 3.3.x](https://www.scala-lang.org) and [sbt 1.9.x](https://www.scala-sbt.org/).

### Contributors

The initial development of this library was started by [Antoine Sébert](https://github.com/AntoineSebert) as a MSc Thesis project at DTU supervised by [Alceste Scalas](https://github.com/alcestes) and co-supervised by [Philip Haller](https://www.csc.kth.se/~phaller/). This fork is a further development of the library.
