package BioGraph

import java.io.File

import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.utilFunctionsObject
import scala.collection.JavaConverters._

/**
  * Created by artem on 28.12.16.
  */
object NextOverlapApp extends App{
  def main(): Unit = {
//    val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val localDB = new File("/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/")
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)
    val names = utilFunctionsObject.getOrganismNames(graphDataBaseConnection)
    names.foreach(utilFunctionsObject.makeNextRelationship(graphDataBaseConnection, "Feature", _))
    names.foreach(utilFunctionsObject.makeOverlapRelationship(graphDataBaseConnection, "Feature", _))
  }
  main()
}
