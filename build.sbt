ThisBuild / version := "0.1.1"
ThisBuild / scalaVersion := "3.9.0"
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
// The forked `run` reads it, but sbt's unused-key lint does not see that.
Global / excludeLintKeys += (run / baseDirectory)

libraryDependencies += "org.scalatest" %% "scalatest" % Versions.scalaTest % Test

lazy val joinActors =
  rootProject.autoAggregate
    .settings(
      name := "joinActors",
      publish / skip := true
    )

lazy val core =
  (project in file("core"))
    .settings(
      name := "core",
      // Verify every `receive` macro expansion in the tests. Not enabled for the main sources:
      // mainargs' own ParserForClass macro fails this check (a quote-scope bug inside mainargs).
      Test / scalacOptions += "-Xcheck-macros",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "mainargs" % Versions.mainargs,
        "org.scalacheck" %% "scalacheck" % Versions.scalaCheck
      )
    )

lazy val benchmarks =
  (project in file("benchmarks"))
    .dependsOn(core)
    .settings(
      name := "benchmarks",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "os-lib" % Versions.osLib,
        "org.jfree" % "jfreechart" % Versions.jfreechart
      ),
      // A fixed heap for stable measurements; `run` gets its own JVM in sbt 2, so .sbtopts
      // no longer applies to it.
      run / javaOptions ++= Seq("-Xms16G", "-Xmx16G"),
      publish / skip := true
    )
