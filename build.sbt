version      := "0.1.1"
scalaVersion := "3.7.1"

lazy val commonDependencies = Seq(
  "com.lihaoyi"         %% "os-lib"     % Versions.osLib,
  "com.lihaoyi"         %% "mainargs"   % Versions.mainargs,
  "com.google.code.gson" % "gson"       % Versions.gson,
  "org.jfree"            % "jfreechart" % "1.5.5"
)
lazy val testDependencies = Seq(
  "org.scalacheck"    %% "scalacheck"         % Versions.scalaCheck,
  "org.scalactic"     %% "scalactic"          % Versions.scalactic         % Test,
  "org.scalatestplus" %% "scalacheck-1-18"    % s"${Versions.scalaTest}.0" % Test,
  "org.scalatest"     %% "scalatest"          % Versions.scalaTest         % Test,
  "org.scalatest"     %% "scalatest-funsuite" % Versions.scalaTest         % Test,
  "org.felher"         % "s3te-compile_3.5"   % "0.0.2"                    % Compile,
  "com.google.guava"   % "guava"              % "33.4.8-jre"               % Compile,
)

// Common settings for all projects
lazy val commonSettings = Seq(
  libraryDependencies ++= commonDependencies ++ testDependencies
)

assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

lazy val commonScalacOptions = Seq("-feature")

lazy val joinActors =
  (project in file("."))
    .aggregate(core, benchmarks)
    .settings(
      name           := "joinActors",
      publish / skip := true,
      assembly / skip := true
    )

lazy val core =
  (project in file("core"))
    .settings(
      name := "core",
      commonSettings,
      scalacOptions ++= commonScalacOptions ++ Seq(), // "-Xcheck-macros"
      assembly / mainClass := Some("core.Main"),
      assembly / assemblyJarName := "joinActors.jar"
    )

lazy val benchmarks =
  (project in file("benchmarks"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(
      name := "benchmarks",
      commonSettings,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      assembly / skip := true
    )
