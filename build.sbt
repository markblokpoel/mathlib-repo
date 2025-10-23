ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.markblokpoel"
ThisBuild / scalaVersion := "2.13.16"

//lazy val runCoherenceDemo = taskKey[Unit]("Run Coherence demonstration.")

lazy val mathlib-repo = (project in file("."))
  .settings(
    // Shared settings
    name := "mathlib-repo",
    autoAPIMappings := true,
//    fullRunTask(runCoherenceDemo, Compile, "mathlib.demos.Coherence", "arg1", "arg2"),
  )


