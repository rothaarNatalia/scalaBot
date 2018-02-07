import sbt._
import Keys._

/**
  * Created by rothaar on 04.02.2018.
  */
object Common {


  val settings: Seq[Def.Setting[_]] = Seq(

      resolvers ++= Seq(
        "Maven Central" at "http://repo1.maven.org/maven2/",
        "mvnrepository" at "http://mvnrepository.com/artifact/",
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        "Typesafe Repository Ivy" at "http://repo.typesafe.com/typesafe/ivy-releases/",
        "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/"
        //Resolver.defaultLocal,
        //Resolver.mavenLocal
      )
  )
}
