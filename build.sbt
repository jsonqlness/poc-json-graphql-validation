name := "playing-with-graphql"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "com.graphql-java" % "graphql-java" % "10.0"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.9.7"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.7"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

logBuffered in Test := false


