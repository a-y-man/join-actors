ThisBuild / version := "0.1.1"
ThisBuild / scalaVersion := "3.8.4"
ThisBuild / scalacOptions += "-feature"

ThisBuild / githubWorkflowJavaVersions := List(JavaSpec.temurin("25"))
ThisBuild / githubWorkflowBuildSbtStepPreamble := Nil
ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("testFull"), name = Some("Build and test"))
)
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowIncludeClean := false

// sbt 2 runs `run` in a separate JVM started in the subproject's directory; keep the repository
// root as working directory, since the Makefile and the code use paths relative to it.
run / baseDirectory := (ThisBuild / baseDirectory).value

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % Versions.scalactic % Test,
  "org.scalatestplus" %% "scalacheck-1-19" % s"${Versions.scalaTest}.0" % Test,
  "org.scalatest" %% "scalatest" % Versions.scalaTest % Test,
  "org.scalatest" %% "scalatest-funsuite" % Versions.scalaTest % Test
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
      // Verify every `receive` macro expansion in the tests. Not enabled for the main sources:
      // mainargs' own ParserForClass macro fails this check (a quote-scope bug inside mainargs).
      Test / scalacOptions += "-Xcheck-macros",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "os-lib" % Versions.osLib,
        "com.lihaoyi" %% "mainargs" % Versions.mainargs,
        "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
      ),
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
      libraryDependencies += "org.jfree" % "jfreechart" % Versions.jfreechart,
      // A fixed heap for stable measurements; `run` gets its own JVM in sbt 2, so .sbtopts
      // no longer applies to it.
      run / javaOptions ++= Seq("-Xms16G", "-Xmx16G"),
      publish / skip := true,
      assembly / skip := true
    )
