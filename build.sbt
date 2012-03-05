name := "collection"

version := "0.1.0"

organization := "com.codecommit"

scalaVersion := "2.9.1"

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// libraryDependencies ++= Seq(
//    "org.specs2" %% "specs2" % "1.8.2" % "test",
//    "org.scalatest" %% "scalatest" % "1.6.1" % "test"
// )

initialCommands in console := """import com.codecommit.collection._"""
