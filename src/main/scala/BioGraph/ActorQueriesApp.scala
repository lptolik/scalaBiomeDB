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
  def main(): Unit = {
    implicit val timeout: Timeout = Timeout(Duration(200, TimeUnit.SECONDS))
    val system = ActorSystem("ScalaBiomeSystem")
    val aqu = ActorQueriesUtil("/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/", system)
//    val organisms = getOrganismNames(aqu.graphDataBaseConnection).drop(6)
//    val orgsAndSeqs = aqu.getOrganismsPolySequences(organisms, "AA_Sequence")
////    val compared = aqu.compareSequences()
////    val compared = aqu.compareSequences(orgsAndSeqs)(aqu.FindSimilarPolyMessage)
//    val identical = aqu.findIdentical(orgsAndSeqs)
//    val compared = aqu.findSimilarSequences(orgsAndSeqs)
//    println("compared passed")
//    val different = aqu.compareSequences(orgsAndSeqs)
//    println("different passed")
    val seqNodes = aqu.getUniProtSequences

    aqu.updateUniProtSequences("/home/artem/work/2017/Timofei/fromUniProtDBNames.txt")(seqNodes)

//    val seqNode = aqu.getUniprotSequenceByID("C5BDE7")//Q8PNT2//A5W4Y8
//    val upu = aqu.updateUniProtSequences(seqNode)
    system.terminate()

//    val uniqueKeys = compared
//      .head
//      ._2
//      .foldLeft(Map[String, List[Any]]())((res, i) => merge(res, i))//.values.flatten.map(a => a.keys).toSet.flatten//.flatten//.map(a => a.toSet)
//    uniqueKeys
  }
  main()
}