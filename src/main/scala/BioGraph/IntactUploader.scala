package BioGraph

import java.io.File


/**
  * Created by artem on 16.06.16.
  */
object IntactUploader {
  def main(args: Array[String]) {
//    val dir = "/home/artem/BLAST_DB/intact/1173018.xml"
    val dir = "/home/artem/BLAST_DB/intact/ecoli/ecoli_04.xml"
    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val ia = new IntactUtil(dir, dataBaseFile)
    val interactors = ia.getInteractors
    val i = interactors.map(ia.interactorInfo)
//    i.foreach(println)
    val reactions = ia.getInteractions
    val r = reactions.map(ia.interactionInfo)
//    r.foreach(println)
    val experiments = ia.getExperiments
    val e = experiments.map(ia.experimentInfo)
//    e.foreach(println)
//    val foundNodes = ia.findPolypetidesInteractors(interactors)
//    foundNodes.foreach(println)
//    println(ia.gbs)
    ia.createInteractorNodes(interactors)
    ia.createReactionsNodes(reactions)
  }
}

