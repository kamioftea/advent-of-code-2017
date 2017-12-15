name := "advent-of-code-2017"

version := "0.1"

scalaVersion := "2.12.4"

enablePlugins(TutPlugin)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "tut"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

resourceDirectory in Tut := baseDirectory.value / "resources"
