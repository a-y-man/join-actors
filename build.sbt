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
      publish / skip := true,
      assembly / skip := true
    )
