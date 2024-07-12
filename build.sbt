ThisBuild / version      := "0.1"
ThisBuild / scalaVersion := "3.3.3"

lazy val scalaTestVersion     = "3.2.18"
lazy val scalaCheckVersion    = "1.17.0"
lazy val scalaTestPlusVersion = "3.2.18.0"
lazy val scalacticVersion     = "3.2.18"
lazy val sourcecodeVersion    = "0.3.1"
lazy val osLibVersion         = "0.10.0"
lazy val mainargsVersion      = "0.7.0"
lazy val gsonVersion          = "2.11.0"

lazy val commonSettings = Seq(
  libraryDependencies += "com.lihaoyi"         %% "os-lib"             % osLibVersion,
  libraryDependencies += "com.lihaoyi"         %% "mainargs"           % mainargsVersion,
  libraryDependencies += "org.scalacheck"      %% "scalacheck"         % scalaCheckVersion,
  libraryDependencies += "org.scalactic"       %% "scalactic"          % scalacticVersion,
  libraryDependencies += "org.scalatestplus"   %% "scalacheck-1-17"    % scalaTestPlusVersion,
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
