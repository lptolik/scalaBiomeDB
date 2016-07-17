package BioGraph

import java.io.File


import psidev.psi.mi.xml.{PsimiXmlReader, PsimiXmlWriter}
import psidev.psi.mi.xml.tutorial.PsimiWriterExample

import scala.collection.JavaConverters._

/**
  * Created by artem on 06.07.16.
  */
object PSICQUICApp {
  def main(args: Array[String]): Unit = {
//    http://www.ebi.ac.uk/Tools/webservices/psicquic/chembl/webservices/psicquic
//    http://tyerslab.bio.ed.ac.uk:8080/psicquic-ws/webservices/psicquic
//    http://matrixdb.ibcp.fr:8080/webservices/psicquic
//    http://mint.bio.uniroma2.it/psicquic-ws-1.1.5/webservices/psicquic
//    http://www.ebi.ac.uk/Tools/webservices/psicquic/mint/webservices
//    http://string.uzh.ch/psicquic/webservices
//    http://www.ebi.ac.uk/Tools/webservices/psicquic/uniprot/webservices
//    http://www.ebi.ac.uk/Tools/webservices/psicquic/reactome/webservices
    val serverAddress = "http://www.ebi.ac.uk/Tools/webservices/psicquic/intact/webservices/psicquic"
    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val psicquic = new PSICQUICUtil(serverAddress)
//    Q9BXW9 P39367 IM-22256-1432 IM-20125-1
    val mitab = psicquic.sendQuery("P76458", 0, 300)
    val xml = psicquic.convertQueryResult(mitab)
    val xmlRead = xml.getEntries.asScala
//
//    val intactUtilObject = new IntactUtil(xmlRead, dataBaseFile)
//    val interactors = intactUtilObject.getInteractors
//    val reactions = intactUtilObject.getInteractions
//    reactions.foreach(println)
//    println(reactions.length)
    val output = new File("/home/artem/work/reps/GenBank/scalaUploadTest/iteractions.xml")
    val writer = new PsimiXmlWriter
    writer.write(xml, output)


  }
}
