
import sbt._
import Keys._

/**
  * Created by rothaar on 04.02.2018.
  */

object Dependencies {

  val commonDependencies: Seq[ModuleID] = Seq(
      "junit" % "junit" % "4.10" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "com.typesafe.akka" %% "akka-http" % "10.0.11",
      "com.typesafe.akka" %% "akka-http-testkit" % "10.0.11" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "com.typesafe.play" %% "play-json" % "2.6.6",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      "com.github.nscala-time" %% "nscala-time" % "2.18.0",
      "info.mukel" %% "telegrambot4s" % "3.0.14",
      "org.scalacheck" %% "scalacheck" % "1.13.3" % "test"

  )

}
