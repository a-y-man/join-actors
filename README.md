# Join-Patterns

Join-Patterns for the Actor Model in Scala 3 using Macros

## Description

This project aims to implement actor-based systems with Join Patterns to process messages in the Scala 3 programming language. This is done by leveraging the metaprogramming facilities of the language to extend the language using its new macro environment and reflection API. Metaprogramming enabled by macros relies on compile time logic to transform and generate code, that will provide a convenient integration into a codebase by leveraging the pattern-matching mechanism of the language.


## Contributors
The initial development of this project was started by [Antoine Sébert](https://github.com/AntoineSebert) as an MSc Thesis project at DTU supervised by [Alceste Scalas](https://github.com/alcestes) and co-supervised by [Philip Haller](https://www.csc.kth.se/~phaller/). This fork is a further development of the project as another MSc thesis project under the same supervisors.


## Built With
The Scala 3 programming language
```scala
scalaVersion := "3.2.0"
```

The [sbt](https://www.scala-sbt.org/) build tool
```scala
sbt.version := "1.7.1"
```


## Usage
To run the tests:
```scala
sbt test
```