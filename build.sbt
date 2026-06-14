version := "0.1.1"
scalaVersion := "3.8.4"
scalacOptions += "-feature"

ThisBuild / githubWorkflowJavaVersions := List(JavaSpec.temurin("25"))

libraryDependencies ++= Seq(
  "com.lihaoyi"          %% "os-lib" % Versions.osLib,
  "com.lihaoyi"          %% "mainargs" % Versions.mainargs,
  "org.scalacheck"       %% "scalacheck" % Versions.scalaCheck,
  "org.scalactic"        %% "scalactic" % Versions.scalactic               % Test,
  "org.scalatestplus"    %% "scalacheck-1-19" % s"${Versions.scalaTest}.0" % Test,
  "org.scalatest"        %% "scalatest" % Versions.scalaTest               % Test,
  "org.scalatest"        %% "scalatest-funsuite" % Versions.scalaTest      % Test,
  "org.felher"            % "s3te-compile_3.5" % "0.0.2" % Compile,
  "com.google.guava"      % "guava" % "33.6.0-jre"       % Compile,
  "org.jfree"             % "jfreechart" % "1.5.5",
  "com.google.code.gson"  % "gson" % Versions.gson,
)


lazy val joinActors =
  rootProject.autoAggregate
    .settings(
      name := "joinActors",
      publish / skip := true,
      assembly / skip := true
    )

lazy val core =
  (project in file("core"))
    .settings(
      name := "core",
      assembly / mainClass := Some("core.Main"),
      assembly / assemblyJarName := "joinActors.jar",
      assembly / assemblyMergeStrategy := {
        case PathList("META-INF", _*) => MergeStrategy.discard
        case _ => MergeStrategy.first
      }
    )

lazy val benchmarks =
  (project in file("benchmarks"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(
      name := "benchmarks",
      publish / skip := true,
      assembly / skip := true
    )
