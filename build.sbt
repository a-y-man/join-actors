ThisBuild / version := "0.0.4"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name                                                := "join_patterns",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging"      % "3.9.5",
    libraryDependencies += "ch.qos.logback"              % "logback-classic"    % "1.4.14",
    libraryDependencies += "com.lihaoyi"                %% "sourcecode"         % "0.3.1",
    libraryDependencies += "org.scalactic"              %% "scalactic"          % "3.2.18",
    libraryDependencies += "org.scalacheck"             %% "scalacheck"         % "1.17.0",
    libraryDependencies += "org.scalatestplus"          %% "scalacheck-1-17"    % "3.2.18.0",
    libraryDependencies += "org.scalatest"              %% "scalatest"          % "3.2.18" % Test,
    libraryDependencies += "org.scalatest"              %% "scalatest-funsuite" % "3.2.18" % Test
  )

scalacOptions ++= Seq("-deprecation", "-feature", "-Xcheck-macros")
