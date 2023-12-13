ThisBuild / version := "0.0.4"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name                                       := "join_patterns",
    libraryDependencies += "org.scalatest"     %% "scalatest"          % "3.2.17" % Test,
    libraryDependencies += "org.scalatest"     %% "scalatest-funsuite" % "3.2.17" % Test,
    libraryDependencies += "org.scalacheck"    %% "scalacheck"         % "1.17.0",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17"    % "3.2.16.0"
  )

scalacOptions ++= Seq("-deprecation", "-feature")
