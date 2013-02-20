import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "StackFlyOver"
    val appVersion      = "1.0-SNAPSHOT"

    

    val appDependencies = Seq(
        "com.h2database" % "h2" % "1.3.167",
        "org.scalaquery" % "scalaquery_2.9.1" % "0.10.0-M1",
        "net.liftweb" % "lift-json_2.9.1" % "2.4",
        "net.databinder" %% "dispatch-http" % "0.8.5",
        "mchv" %% "play2-quartz" % "1.1",
        "org.apache.commons" % "commons-lang3" % "3.1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
        // Add your own project settings here      
        scalacOptions ++= Seq( "-deprecation", "-optimize" ),
        resolvers += "Mariot Chauvin" at "http://mchv.me/repository"    
    )

}
