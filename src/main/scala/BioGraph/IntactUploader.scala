package BioGraph

import java.io.File

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import psidev.psi.mi.xml.PsimiXmlReader
import scala.collection.JavaConverters._

/**
  * Created by artem on 16.06.16.
  */
object IntactUploader {
  def main(args: Array[String]) {

//    val pathToPsiXmlFiles = "/home/artem/BLAST_DB/intact/ecoli/"
    val pathToPsiXmlFiles = "/home/jane/graph_new_release/PPI/"
//    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val dataBaseFile = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)

    val reader = new PsimiXmlReader()

    val psiXmlFiles = utilFunctions.utilFunctionsObject.getUploadFilesFromDirectory(pathToPsiXmlFiles, "xml")

    def uploadOnePsiXml(PsiXmlFile: File): Unit = {
      println(PsiXmlFile.getName)
      val readResult = reader.read(PsiXmlFile).getEntries.asScala
      val intactUtilObject = new IntactUtil(readResult)
      val interactors = intactUtilObject.getInteractors
      val i = interactors.map(intactUtilObject.interactorInfo)

      val reactions = intactUtilObject.getInteractions
      val r = reactions.map(intactUtilObject.interactionInfo)

//      val experiments = intactUtilObject.getExperiments
//      val e = experiments.map(intactUtilObject.experimentInfo)

      intactUtilObject.createInteractorNodes(interactors, graphDataBaseConnection)
      intactUtilObject.createReactionsNodes(reactions, graphDataBaseConnection)
    }
    psiXmlFiles.foreach(uploadOnePsiXml)
  }
}

