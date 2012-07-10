// set the name of the project
name := "Critical Mass"

version := "1.0"

organization := "org.seacourt"

scalaVersion := "2.9.1"

//resolvers ++= Seq(
//    "Java Maven2 Repo" at "http://download.java.net/maven/2",
//    "Jerkson repo" at "http://repo.codahale.com",
//    "Jackson repo" at "http://repository.codehaus.org/org/codehaus"
//)

//libraryDependencies += "com.codahale" % "jerkson_2.9.1" % "0.5.0"

libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.5"

libraryDependencies += "net.liftweb" % "lift-json_2.9.1" % "2.4"


