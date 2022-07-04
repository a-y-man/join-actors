ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name                                   := "join_patterns",
    libraryDependencies += "org.scalatest" %% "scalatest"          % "3.2.12" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.12" % "test"
  )
