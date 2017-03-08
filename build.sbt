import sbtassembly.AssemblyPlugin.autoImport._

name := "BioGraph"

version := "3.0"

scalaVersion := "2.11.7"

resolvers += "European Bioinformatics Institute" at "http://www.ebi.ac.uk/intact/maven/nexus/content/groups/public"
//http://www.ebi.ac.uk/~maven/m2repo/org/sbml/jsbml/jsbml/
//http://www.ebi.ac.uk/~maven/m2repo
//resolvers += "The EBI repository" at "http://www.ebi.ac.uk/~maven/m2repo/org/sbml/"

//resolvers += "Biojava3" at "http://www.biojava.org/download/maven"

resolvers += "The JSBML repository" at "http://jsbml.sourceforge.net/m2repo_snapshots"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test"

libraryDependencies += "org.neo4j" % "neo4j" % "2.3.1"

//libraryDependencies += "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.5"

//libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.5"

libraryDependencies += "org.biojava" % "biojava-core" % "4.2.0"

libraryDependencies += "org.biojava" % "sequencing" % "1.9.2"

libraryDependencies += "org.neo4j" % "neo4j-kernel" % "2.3.1" classifier "tests"

libraryDependencies += "org.neo4j" % "neo4j-io" % "2.3.1" classifier "tests"

libraryDependencies += "psidev.psi.mi" % "psi25-xml" % "1.8.4" % "provided"

libraryDependencies += "psidev.psi.mi" % "psimitab" % "1.8.4" % "provided"

libraryDependencies += "org.hupo.psi.mi.psicquic" % "psicquic-client" % "1.5.3"  % "provided"

//libraryDependencies += "org.sbml.jsbml.modules" % "jsbml-tidy" % "1.1-b1"

//libraryDependencies += "com.graphaware.neo4j" % "tests" % "3.0.3.39"

libraryDependencies += "org.sbml.jsbml" % "jsbml" % "1.2-SNAPSHOT"

libraryDependencies += "org.sbml.jsbml.ext" % "jsbml-fbc" % "1.2-SNAPSHOT"

//sbt-assembly
//assemblySettings
assemblyMergeStrategy in assembly := {
  case PathList("org", "neo4j", xs @ _*)           => MergeStrategy.first
  case PathList("org", "bouncycastle", xs @ _*)    => MergeStrategy.first
  case PathList("org", "sbml", xs @ _*)           => MergeStrategy.first
  case PathList("org", "biojava", xs @ _*)           => MergeStrategy.first
  case PathList("org", "apache", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "apache", "servicemix", "specs", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "apache", "xalan", xs @ _*)    => MergeStrategy.first
//  case PathList("xalan", xs @ _*)    => MergeStrategy.first
  case PathList("org", "codehaus", xs @ _*)    => MergeStrategy.first
  case PathList("org", "hupo", xs @ _*)    => MergeStrategy.first
  case PathList("pom.xml") => MergeStrategy.first
//  case PathList("org", "bouncycastle", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "springframework", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "springsource", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "objectweb", xs @ _*)    => MergeStrategy.first
  case PathList("META-INF", "LICENSES.txt") => MergeStrategy.discard
  case "overview.html" => MergeStrategy.first
  case "log4j2.xml"                          => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}