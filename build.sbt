ThisBuild / version      := "0.1"
ThisBuild / scalaVersion := "3.5.2"

lazy val scalaTestVersion     = "3.2.19"
lazy val scalaCheckVersion    = "1.18.1"
lazy val scalaTestPlusVersion = "3.2.19.0"
lazy val scalacticVersion     = "3.2.19"
lazy val sourcecodeVersion    = "0.4.3"
lazy val osLibVersion         = "0.11.3"
lazy val mainargsVersion      = "0.7.6"
lazy val gsonVersion          = "2.11.0"

lazy val commonSettings = Seq(
  libraryDependencies += "com.lihaoyi"         %% "os-lib"             % osLibVersion,
  libraryDependencies += "com.lihaoyi"         %% "mainargs"           % mainargsVersion,
  libraryDependencies += "org.scalacheck"      %% "scalacheck"         % scalaCheckVersion,
  libraryDependencies += "org.scalactic"       %% "scalactic"          % scalacticVersion,
  libraryDependencies += "org.scalatestplus"   %% "scalacheck-1-18"    % scalaTestPlusVersion,
  libraryDependencies += "com.google.code.gson" % "gson"               % gsonVersion,
  libraryDependencies += "org.scalatest"       %% "scalatest"          % scalaTestVersion % Test,
  libraryDependencies += "org.scalatest"       %% "scalatest-funsuite" % scalaTestVersion % Test
)

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

lazy val commonScalacOptions = Seq("-feature")

lazy val joinActors =
  (project in file("."))
    .aggregate(core, benchmarks)
    .settings(
      name         := "joinActors",
      version      := version.value,
      scalaVersion := scalaVersion.value
    )

lazy val core =
  (project in file("core"))
    .settings(
      name := "core",
      commonSettings,
      scalacOptions ++= commonScalacOptions ++ Seq("Xcheck-macros"),
      assembly / assemblyJarName := "joinActors.jar"
    )

lazy val benchmarks =
  (project in file("benchmarks"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(
      name := "benchmarks",
      commonSettings,
      scalacOptions ++= commonScalacOptions
    )
