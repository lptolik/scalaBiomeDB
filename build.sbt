import sbtassembly.AssemblyPlugin.autoImport._

name := "BioGraph"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test"

libraryDependencies += "org.neo4j" % "neo4j" % "2.3.1"

//libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"

//libraryDependencies += "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.5"

//libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.5"

libraryDependencies += "org.biojava" % "biojava-core" % "4.2.0"

//sbt-assembly
//assemblySettings
assemblyMergeStrategy in assembly := {
  case PathList("neo4j", xs @ _*) =>
    MergeStrategy.last
  case PathList("META-INF", "LICENSES.txt") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}