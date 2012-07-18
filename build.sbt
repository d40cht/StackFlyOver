// set the name of the project
name := "Critical Mass"

version := "1.0"

organization := "org.seacourt"

scalaVersion := "2.9.1"

libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.5"

libraryDependencies += "net.liftweb" % "lift-json_2.9.1" % "2.4"

libraryDependencies += "com.h2database" % "h2" % "1.3.167"

libraryDependencies += "org.scalaquery" % "scalaquery_2.9.1" % "0.10.0-M1"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

//libraryDependencies += "com.vividsolutions" % "jts" % "1.12"

scalacOptions += "-deprecation"

scalacOptions += "-optimize"

