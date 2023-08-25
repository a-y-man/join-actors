ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name                                       := "join_patterns",
    libraryDependencies += "org.scalatest"     %% "scalatest"          % "3.2.12"   % Test,
    libraryDependencies += "org.scalatest"     %% "scalatest-funsuite" % "3.2.12"   % "test",
    libraryDependencies += "org.scalacheck"    %% "scalacheck"         % "1.17.0"   % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17"    % "3.2.16.0" % "test"
  )

scalacOptions ++= Seq("-deprecation", "-feature")
