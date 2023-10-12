ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "Collection SPL",
      idePackagePrefix := Some("de.uni_saarland.cs.se")
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.10" % "test"