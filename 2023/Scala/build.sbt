ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2023",
    idePackagePrefix := Some("de.mr_pine.aoc.y2023")
  )
