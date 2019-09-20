package BioGraph

import java.io.File

import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.utilFunctionsObject

/**
  * Created by artem on 28.12.16.
  */
object NextOverlapApp extends App{
  def main(configurationFilename: String = "/home/artem/work/2017/Timofei/next_overlap_config.txt"): Unit = {
//    val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
//    val remoteDB = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")
    val conf = utilFunctionsObject.readConfigurationFile(configurationFilename)
    val dbPath = conf(0)
    val featureType = conf(1)
    val localDB = new File(dbPath)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)
    val names = utilFunctionsObject.getOrganismNames(graphDataBaseConnection)//.filter(_ == "Escherichia coli str. K-12 substr. MG1655")
    names.foreach(utilFunctionsObject.makeNextRelationship(graphDataBaseConnection, featureType, _))
    names.foreach(utilFunctionsObject.makeOverlapRelationship(graphDataBaseConnection, featureType, _))
  }
//  val localDB = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
//  val remoteDB = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"
//  main("/home/artem/work/2018/staphylococcus/next_overlap_config.txt")
  main("/Users/lptolik/Documents/Projects/Liverpool/Penicillium/config/penicillium_gems/next_overlap_config.txt")
}
