package BioGraph

import java.io.File

import psidev.psi.mi.xml.PsimiXmlReader
import scala.collection.JavaConverters._

/**
  * Created by artem on 16.06.16.
  */
object IntactUploader {
  def main(args: Array[String]) {
//    val dir = "/home/artem/BLAST_DB/intact/1173018.xml"
    val pathToPsiXml = "/home/artem/BLAST_DB/intact/ecoli/ecoli_01.xml"
    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val fileOfPsiXml = new File(pathToPsiXml)
    val reader = new PsimiXmlReader()
    val readResult = reader.read(fileOfPsiXml).getEntries.asScala
    val intactUtilObject = new IntactUtil(readResult, dataBaseFile)
    val interactors = intactUtilObject.getInteractors
    val i = interactors.map(intactUtilObject.interactorInfo)
//    i.foreach(println)
    val reactions = intactUtilObject.getInteractions
    val r = reactions.map(intactUtilObject.interactionInfo)
//    r.foreach(println)
    val experiments = intactUtilObject.getExperiments
    val e = experiments.map(intactUtilObject.experimentInfo)
//    e.foreach(println)
//    val foundNodes = intactUtilObject.findPolypetidesInteractors(interactors)
//    foundNodes.foreach(println)
//    println(intactUtilObject.gbs)
    intactUtilObject.createInteractorNodes(interactors)
    intactUtilObject.createReactionsNodes(reactions)
  }
}

