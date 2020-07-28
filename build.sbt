import sbtassembly.AssemblyPlugin.autoImport._

name := "BioGraph"

version := "3.0.3"

scalaVersion := "2.11.7"

resolvers += "ebi-repo" at "https://www.ebi.ac.uk/intact/maven/nexus/content/repositories/ebi-repo/"

//resolvers += "spring-plugins" at "http://repo.spring.io/plugins-release/"

resolvers += "biopax" at "http://www.biopax.org/m2repo/releases/"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test"

libraryDependencies += "javax.ws.rs" % "javax.ws.rs-api" % "2.1.1" artifacts( Artifact("javax.ws.rs-api", "jar", "jar"))

libraryDependencies += "org.neo4j" % "neo4j" % "3.5.20"

// https://mvnrepository.com/artifact/org.neo4j.community/it-test-support
libraryDependencies += "org.neo4j.community" % "it-test-support" % "3.5.20" % Test

//libraryDependencies += "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.5"

//libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.5"

libraryDependencies += "org.biojava" % "biojava-core" % "5.3.0"

libraryDependencies += "org.biojava" % "sequencing" % "1.9.2"

libraryDependencies += "org.neo4j" % "neo4j-kernel" % "3.5.20" classifier "tests"

libraryDependencies += "org.neo4j" % "neo4j-io" % "3.5.20" classifier "tests"

// https://mvnrepository.com/artifact/psidev.psi.mi/psi25-xml
libraryDependencies += "psidev.psi.mi" % "psi25-xml" % "1.8.4" % "provided"

// https://mvnrepository.com/artifact/psidev.psi.mi/psimitab
libraryDependencies += "psidev.psi.mi" % "psimitab" % "1.8.4" % "provided"

libraryDependencies += "org.hupo.psi.mi.psicquic" % "psicquic-client" % "1.5.3"  % "provided"

//libraryDependencies += "org.sbml.jsbml.modules" % "jsbml-tidy" % "1.1-b1"

//libraryDependencies += "com.graphaware.neo4j" % "tests" % "3.0.3.39"

libraryDependencies += "org.sbml.jsbml" % "jsbml" % "1.2"

libraryDependencies += "org.sbml.jsbml.ext" % "jsbml-fbc" % "1.2"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.5.1"

libraryDependencies += "uk.ac.ebi.uniprot" % "japi" % "1.0.25" % "provided"

//sbt-assembly
//assemblySettings
assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard 
  case PathList("org", "neo4j", xs @ _*)           => MergeStrategy.first
//  case PathList("org", "bouncycastle", xs @ _*)    => MergeStrategy.last
  case PathList("org", "sbml", xs @ _*)           => MergeStrategy.first
  case PathList("org", "biojava", xs @ _*)           => MergeStrategy.first
  case PathList("org", "apache", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "apache", "servicemix", "specs", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "apache", "xalan", xs @ _*)    => MergeStrategy.first
//  case PathList("xalan", xs @ _*)    => MergeStrategy.first
  case PathList("org", "codehaus", xs @ _*)    => MergeStrategy.first
  case PathList("com", "ctc", xs @ _*)    => MergeStrategy.last
  case PathList("org", "slf4j", xs @ _*)    => MergeStrategy.last
  case PathList("org", "hamcrest", xs @ _*)    => MergeStrategy.last
  case PathList("org", "aopalliance", xs @ _*)    => MergeStrategy.last
  case PathList("javax", "inject", xs @ _*)    => MergeStrategy.last
  case PathList("javax", "ws", xs @ _*)    => MergeStrategy.last
  case PathList("org", "hupo", xs @ _*)    => MergeStrategy.first
  case PathList("pom.xml") => MergeStrategy.first
//  case PathList("bcpkix-jdk15on-1.64.jar", xs @ _*) => MergeStrategy.last
//  case PathList("bcprov-jdk15on-1.64.jar", xs @ _*) => MergeStrategy.last
//  case PathList("org", "bouncycastle", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "springframework", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "springsource", xs @ _*)    => MergeStrategy.first
//  case PathList("org", "objectweb", xs @ _*)    => MergeStrategy.first
  case PathList("META-INF", "LICENSES.txt") => MergeStrategy.discard
  case "overview.html" => MergeStrategy.first
  case "log4j2.xml"                          => MergeStrategy.first
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)

}

mainClass in assembly := Some("BioGraph.sbml.JSBMLUpload")
//mainClass in assembly := Some("BioGraph.PEGIDsUploadApp")
//mainClass in assembly := Some("BioGraph.sbml.Test")
