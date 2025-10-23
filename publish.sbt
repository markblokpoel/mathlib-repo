ThisBuild / organization := "com.markblokpoel"
ThisBuild / organizationName := "markblokpoel"
ThisBuild / organizationHomepage := Some(url("https://markblokpoel.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/markblokpoel/mathlib-repo"),
    "scm:git@github.com:markblokpoel/mathlib-repo.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "markblokpoel",
    name = "Mark Blokpoel",
    email = "mark.blokpoel@gmail.com",
    url = url("https://markblokpoel.com")
  )
)

ThisBuild / description := "mathlib-repo is a repository of mathlib projects used in publications."
ThisBuild / licenses := List(
  "GNU General Public License v3.0" -> new URL("https://github.com/markblokpoel/mathlib/blob/main/LICENSE")
)
ThisBuild / homepage := Some(url("https://markblokpoel.com/mathlib-repo"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true

// new setting for the Central Portal
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
