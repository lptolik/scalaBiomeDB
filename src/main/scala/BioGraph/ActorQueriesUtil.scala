package BioGraph

/**
  * Created by artem on 17.05.17.
  */
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import utilFunctions.WorkWithGraph

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import akka.pattern.ask
import akka.util.Timeout
import utilFunctions.utilFunctionsObject._

case object SeekPolySequenceMessage
case object ComparePolyMessage

class ActorQueriesUtil(pathToDataBase: String, system: ActorSystem)(implicit timeout: Timeout) extends WorkWithGraph(pathToDataBase) {

  type Neo4jNode = org.neo4j.graphdb.Node

  class OrganismPolySequenceSeeker(organismName: String) extends Actor {

    def seekPolySequences: Map[String, List[AnyRef]] = transaction(graphDataBaseConnection) {
      val query =
        s"MATCH (:Organism{name:'$organismName'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(seq:AA_Sequence)" +
          "RETURN seq"
      val sequences = graphDataBaseConnection.execute(query).asScala.toList
      val nodes = sequences.map(e => e.get("seq"))
      Map(organismName -> nodes)
    }

    def receive = {
      case SeekPolySequenceMessage => sender ! seekPolySequences
      case _ => sender ! Map[String, List[AnyRef]]()
    }
  }

  class OrganismSequenceMatchSeeker(fromOrganism: (String, List[Neo4jNode]), toOrganism: (String, List[Neo4jNode])) extends Actor {

    def compareSeq(): List[Map[String, List[Neo4jNode]]] = transaction(graphDataBaseConnection) {
      val res = fromOrganism._2.map(
        from => toOrganism._2.map(
          to => s"${fromOrganism._1} -> ${toOrganism._1}" -> utilFunctions.utilFunctionsObject.matchRelationExistenceWithoutDirection(from, to)
          ).filter(_._2.nonEmpty)
          .toMap
        ).filter(_.nonEmpty)
      res
    }

    def receive = {
      case ComparePolyMessage => sender ! compareSeq()
      case _ => sender ! Map[String, List[AnyRef]]()
    }
  }

  def getOrganismsPolySequences(organismList: List[String]):Map[String, List[Neo4jNode]] = {
    val organismActors = organismList.map(name => system.actorOf(Props(new OrganismPolySequenceSeeker(name))))
    val futureResults = organismActors.map(ask(_, SeekPolySequenceMessage).mapTo[Map[String, List[Neo4jNode]]])
    val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS))).toMap
    organismActors.foreach(system.stop)
    res
  }

//  def compareSequences() = {
  def compareSequences(mapOfOrganismsAndSequences: Map[String, List[Neo4jNode]]) = {

    def loop(hd: (String, List[Neo4jNode]), tl: Map[String, List[Neo4jNode]], matched: Map[String, List[Map[String, List[Neo4jNode]]]]): Map[String, List[Map[String, List[Neo4jNode]]]] = {
      if (tl.nonEmpty) {
        val actors = tl.map(t => system.actorOf(Props(new OrganismSequenceMatchSeeker(hd, t))))
        val futureResults = actors.map(ask(_, ComparePolyMessage).mapTo[List[Map[String, List[Neo4jNode]]]])//.mapTo[List[Neo4jNode]])
        val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS))).toList
        actors.foreach(system.stop)
        loop(tl.head, tl.tail, matched ++ Map(hd._1 -> res))
      }
      else {
        matched
      }
    }

    val mapOfMathced = loop(mapOfOrganismsAndSequences.head, mapOfOrganismsAndSequences.tail, Map())

    def merge[T <: AnyRef, U >: AnyRef](m: Map[T, List[U]], n: Map[T, List[U]]): Map[T, List[U]] = {
      val keyIntersection = m.keySet & n.keySet
      val r1 = keyIntersection.map(key => key -> (m(key) ++ n(key)))
      //      val r1 = for(key <- keyIntersection) yield key -> (m(key) :: List(n(key)))
      val r2 = m.filterKeys(!keyIntersection.contains(_)) ++ n.filterKeys(!keyIntersection.contains(_))
      r2 ++ r1
    }

//    res.map(org => org)
    val res = mapOfMathced.map(e => e._2.foldLeft(Map[String, List[AnyRef]]())((foldRes, i) => merge(foldRes, i)))
    res
  }

}
