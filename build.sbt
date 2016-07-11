import sbtassembly.AssemblyPlugin.autoImport._

name := "BioGraph"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "European Bioinformatics Institute" at "http://www.ebi.ac.uk/intact/maven/nexus/content/groups/public"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test"

libraryDependencies += "org.neo4j" % "neo4j" % "2.3.1"

//libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"

//libraryDependencies += "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.5"

//libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.5"

libraryDependencies += "org.biojava" % "biojava-core" % "4.2.0"

libraryDependencies += "org.biojava" % "sequencing" % "1.9.2"

libraryDependencies += "org.neo4j" % "neo4j-kernel" % "2.3.1" classifier "tests"

libraryDependencies += "org.neo4j" % "neo4j-io" % "2.3.1" classifier "tests"

libraryDependencies += "psidev.psi.mi" % "psi25-xml" % "1.8.4"

libraryDependencies += "psidev.psi.mi" % "psimitab" % "1.8.4"

libraryDependencies += "org.hupo.psi.mi.psicquic" % "psicquic-client" % "1.5.3"

//sbt-assembly
//assemblySettings
assemblyMergeStrategy in assembly := {
  case PathList("neo4j", xs @ _*)           => MergeStrategy.last
  case PathList("META-INF", "LICENSES.txt") => MergeStrategy.discard
  case "log4j2.xml"                          => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}