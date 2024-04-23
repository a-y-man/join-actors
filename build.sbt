ThisBuild / version      := "0.0.5"
ThisBuild / scalaVersion := "3.3.3"

lazy val scalaTestVersion     = "3.2.18"
lazy val scalaCheckVersion    = "1.17.0"
lazy val scalaTestPlusVersion = "3.2.18.0"
lazy val scalacticVersion     = "3.2.18"
lazy val scalaLoggingVersion  = "3.9.5"
lazy val logbackVersion       = "1.4.14"
lazy val upickleVersion       = "3.2.0"
lazy val sourcecodeVersion    = "0.3.1"
lazy val osLibVersion         = "0.9.3"
lazy val mainargsVersion      = "0.7.0"

lazy val commonSettings = Seq(
  libraryDependencies += "com.lihaoyi"       %% "upickle"            % upickleVersion,
  libraryDependencies += "com.lihaoyi"       %% "sourcecode"         % sourcecodeVersion,
  libraryDependencies += "com.lihaoyi"       %% "os-lib"             % osLibVersion,
  libraryDependencies += "com.lihaoyi"       %% "mainargs"           % mainargsVersion,
  libraryDependencies += "org.scalacheck"    %% "scalacheck"         % scalaCheckVersion,
  libraryDependencies += "org.scalactic"     %% "scalactic"          % scalacticVersion,
  libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17"    % scalaTestPlusVersion,
  libraryDependencies += "org.scalatest"     %% "scalatest"          % scalaTestVersion % Test,
  libraryDependencies += "org.scalatest"     %% "scalatest-funsuite" % scalaTestVersion % Test
)

scalacOptions ++= Seq("-deprecation", "-feature", "-Xcheck-macros")

lazy val core =
  (project in file("core"))
    .settings(
      name := "core",
      commonSettings
    )

lazy val benchmarks =
  (project in file("benchmarks"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(
      name := "benchmarks",
      commonSettings
    )
