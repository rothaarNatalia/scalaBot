name := "scalaBot"

version := "1.1"

scalaVersion := "2.12.1"


lazy val scalaBot = (project in file(".")).
        settings(libraryDependencies ++= Dependencies.commonDependencies).
        settings(Common.settings)
