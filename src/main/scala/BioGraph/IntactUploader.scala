package BioGraph


/**
  * Created by artem on 16.06.16.
  */
object IntactUploader {
  def main(args: Array[String]) {
    val dir = "/home/artem/BLAST_DB/intact/1173018.xml"
    val ia = new IntactUtil(dir)
    val interactors = ia.getInteractors
    val i = interactors.head.map(ia.interactorInfo)
    i.foreach(println)
    val reactions = ia.getInteractions
    val r = reactions.head.map(ia.interactionInfo)
    r.foreach(println)
    val experiments = ia.getExperiments
    val e = experiments.head.map(ia.experimentInfo)
    e.foreach(println)
  }
}

