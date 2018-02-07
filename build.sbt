name := "scalaBot"

version := "1.0"

scalaVersion := "2.12.1"


lazy val scalaBot = (project in file(".")).
        settings(libraryDependencies ++= Dependencies.commonDependencies).
        settings(Common.settings)

/*
libraryDependencies ++=    Seq("junit" % "junit" % "4.10" % "test",
"org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
"com.typesafe.akka" %% "akka-http" % "10.0.11",
"com.typesafe.akka" %% "akka-http-testkit" % "10.0.11" % Test)
*/


libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "com.typesafe.akka" %% "akka-http" % "10.0.11",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.11" % Test,
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "com.typesafe.play" %% "play-json" % "2.6.6"
)
