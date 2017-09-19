package BioGraph

import java.io.File

import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.utilFunctionsObject

/**
  * Created by artem on 28.12.16.
  */
object NextOverlapApp extends App{
  def main(dbPath: String, featureType: String = "Feature"): Unit = {
//    val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
//    val remoteDB = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")
    val localDB = new File(dbPath)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)
    val names = utilFunctionsObject.getOrganismNames(graphDataBaseConnection).filter(_ == "Escherichia coli str. K-12 substr. MG1655")
    names.foreach(utilFunctionsObject.makeNextRelationship(graphDataBaseConnection, featureType, _))
    names.foreach(utilFunctionsObject.makeOverlapRelationship(graphDataBaseConnection, featureType, _))
  }
  val localDB = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
  val remoteDB = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"
  main(remoteDB)
}
