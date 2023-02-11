ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name                                   := "join_patterns",
    libraryDependencies += "org.scalatest" %% "scalatest"          % "3.2.12" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.12" % "test"
  )

scalacOptions ++= Seq("-deprecation", "-feature")
