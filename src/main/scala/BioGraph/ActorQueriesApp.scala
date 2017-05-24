package BioGraph

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import org.neo4j.graphdb.DynamicLabel

import scala.concurrent.duration.Duration

/**
  * Created by artem on 19.05.17.
  */
import utilFunctions.utilFunctionsObject.getOrganismNames

object ActorQueriesApp extends App {
  def main() = {
    implicit val timeout = Timeout(Duration(200, TimeUnit.SECONDS))
    val system = ActorSystem("ScalaBiomeSystem")
    val aqu = new ActorQueriesUtil("/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/", system)
    val organisms = getOrganismNames(aqu.graphDataBaseConnection)
    val orgsAndSeqs = aqu.getOrganismsPolySequences(organisms)
//    val compared = aqu.compareSequences()
    val compared = aqu.compareSequences(orgsAndSeqs)
    system.terminate()
    compared

//    val uniqueKeys = compared
//      .head
//      ._2
//      .foldLeft(Map[String, List[Any]]())((res, i) => merge(res, i))//.values.flatten.map(a => a.keys).toSet.flatten//.flatten//.map(a => a.toSet)
//    uniqueKeys
  }
  val res = main()
}