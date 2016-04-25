name := "slacc"

version := "1.0"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"