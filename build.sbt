ThisBuild / version := "0.1.5"
ThisBuild / organization := "com.markblokpoel"
ThisBuild / scalaVersion := "2.13.16"

//lazy val runCoherenceDemo = taskKey[Unit]("Run Coherence demonstration.")

lazy val mathlibRepo = (project in file("."))
  .settings(
    // Shared settings
    name := "mathlib-repo",
    libraryDependencies += "com.markblokpoel" %% "mathlib" % "0.9.3",
    autoAPIMappings := true,
  )


