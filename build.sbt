ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.markblokpoel"
ThisBuild / scalaVersion := "2.13.16"

//lazy val runCoherenceDemo = taskKey[Unit]("Run Coherence demonstration.")

lazy val mathlibRepo = (project in file("."))
  .settings(
    // Shared settings
    name := "mathlib-repo",
    libraryDependencies += "com.markblokpoel" %% "mathlib" % "0.9.3",
    autoAPIMappings := true,
//    fullRunTask(runCoherenceDemo, Compile, "mathlib.demos.Coherence", "arg1", "arg2"),
  )


