package BioGraph


import java.io.File

import BioGraph._
import org.neo4j.graphdb.factory.GraphDatabaseFactory

/**
  * Created by artem on 10.06.16.
  */
object SeedUploader {
  def main(args: Array[String]) {
    val seedFile = new File("/home/artem/work/2016/metagenome/511145.6.gff")
    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val seedUploader = new SeedUtil(seedFile, dataBaseFile)
    seedUploader.createSeedXrefs()
  }

}
