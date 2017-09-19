package BioGraph

/**
  * Created by artem on 17.05.17.
  */
import java.util.concurrent.TimeUnit

import BioGraph.{DBNode, XRef}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import utilFunctions.WorkWithGraph

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import akka.pattern.ask
import akka.util.Timeout
import org.neo4j.consistency.ConsistencyCheckService.Result
import org.neo4j.graphdb.{Direction, DynamicLabel}
import uk.ac.ebi.kraken.interfaces.uniprot.DatabaseCrossReference
import uk.ac.ebi.uniprot.dataservice.client.Client
import uk.ac.ebi.uniprot.dataservice.client.uniprot.UniProtQueryBuilder
import utilFunctions._

import scala.collection.convert.Wrappers.SeqWrapper
import scala.collection.parallel.mutable.ParMap
import scala.io.Source

case class ActorQueriesUtil(pathToDataBase: String, system: ActorSystem)(implicit timeout: Timeout) extends WorkWithGraph(pathToDataBase) {

  type Neo4jNode = org.neo4j.graphdb.Node

  trait Message

  case object SeekPolySequenceMessage extends Message
  case object ComparePolyMessage extends Message
  case object MatchSequencesDifferenceMessage extends Message
  case object CypherQuery extends Message
  case object UpdateUniprot extends Message

  var externalDataBasesCollector = transaction(graphDataBaseConnection) {
    graphDataBaseConnection
      .findNodes(DynamicLabel.label("DB"))
      .asScala
      .map{n=>
        val name = n.getProperty("name").toString
        name.toUpperCase -> DBNode(name, nodeId = n.getId)}
      .toMap
      .par
  }

  case class OrganismPolySequenceSeeker(organismName: String) extends Actor {

    def seekPolySequences: Map[String, List[AnyRef]] = transaction(graphDataBaseConnection) {
      val query =
        s"MATCH (:Organism{name:'$organismName'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(seq:AA_Sequence)" +
          "RETURN COLLECT(seq)"
      val sequences = graphDataBaseConnection.execute(query).asScala.toList
      val nodes = sequences.map(e => e.get("COLLECT(seq)"))
      Map(organismName -> nodes)
    }

    def receive = {
      case SeekPolySequenceMessage => sender ! seekPolySequences
      case _ => sender ! Map[String, List[AnyRef]]()
    }
  }

  case class OrganismSequenceMatchSeeker(fromOrganism: (String, List[Neo4jNode]), toOrganism: (String, List[Neo4jNode])) extends Actor {

    def compareSeq(): List[Map[String, List[Neo4jNode]]] = transaction(graphDataBaseConnection) {
      val res = fromOrganism._2.map(
        from => toOrganism._2.map(
          to => s"${fromOrganism._1} -> ${toOrganism._1}" ->
            utilFunctions.utilFunctionsObject.matchRelationExistenceWithoutDirection(from, to)
          )
          .filter(_._2.nonEmpty)
          .toMap
        )//.filter(_.nonEmpty)
      res
    }

    def findDifference(): Map[String, List[Neo4jNode]] = transaction(graphDataBaseConnection) {
      val queryForSimilarity = s"MATCH (:Organism{name:'${fromOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s1:Sequence)-[:SIMILAR]-(s2:Sequence)<-[:IS_A]-(:Polypeptide)-[:PART_OF]->(:Organism{name:'${toOrganism._1}'})" +
        s" RETURN COLLECT(DISTINCT s1), COLLECT(DISTINCT s2)"
      val queryForIdenticalSequences = s"MATCH (:Organism{name:'${fromOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s1:Sequence)<-[:IS_A]-(:Polypeptide)-[:PART_OF]->(:Organism{name:'${toOrganism._1}'})" +
        s" RETURN COLLECT(DISTINCT s1)"

      val queryFromOrgamismSequences = s"MATCH (:Organism{name:'${fromOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s1:Sequence)" +
        s" RETURN COLLECT(DISTINCT s1)"
      val queryToOrgamismSequences = s"MATCH (:Organism{name:'${toOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s2:Sequence)" +
        s" RETURN COLLECT(DISTINCT s2)"

      val similarSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryForSimilarity)))
      val identicalSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryForIdenticalSequences)))
      val fromSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryFromOrgamismSequences)))
      val toSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryToOrgamismSequences)))

      val similarSequencesRes = getCollectionOfNodes(similarSequencesActor, CypherQuery)()
      val identicalSequences = getCollectionOfNodes(identicalSequencesActor, CypherQuery)()
        .values
        .head
        .asInstanceOf[SeqWrapper[Neo4jNode]]
        .asScala
        .toSet

      val fromSequences = getCollectionOfNodes(fromSequencesActor, CypherQuery)()
        .values
        .head
        .asInstanceOf[SeqWrapper[Neo4jNode]]
        .asScala
        .toSet

      val toSequences = getCollectionOfNodes(toSequencesActor, CypherQuery)()
        .values
        .head
        .asInstanceOf[SeqWrapper[Neo4jNode]]
        .asScala
        .toSet

      val allSimilarSequences = similarSequencesRes
        .values
        .map(_.asInstanceOf[SeqWrapper[Neo4jNode]])
        .map(_.asScala.toSet)
      val fromSimilarSequences = allSimilarSequences.head
      val toSimilarSequences = allSimilarSequences.last

      val similarSequencesIntersection = fromSimilarSequences.union(toSimilarSequences).union(identicalSequences)
      val fromDifference = similarSequencesIntersection.diff(fromSequences)
      val toDifference = similarSequencesIntersection.diff(toSequences)
      val res = Map(fromOrganism._1 -> fromDifference.toList, toOrganism._1 -> toDifference.toList)
      res
    }

    def receive = {
      case ComparePolyMessage => sender ! compareSeq()
      case MatchSequencesDifferenceMessage => sender ! findDifference()
      case _ => sender ! Map[String, List[AnyRef]]()
    }
  }

  case class CypherQueryExecutor(query: String) extends Actor{
    def receive = {
      case CypherQuery => sender ! {
        val res = graphDataBaseConnection.execute(query).asScala//.toList
        res.next.asScala.toMap
      }
      case _ => sender ! List()
    }
  }

  private def getCollectionOfNodes(actor: ActorRef, message: Message)(duration: Duration = Duration(100, TimeUnit.SECONDS)) = {
    val futureResults = actor.ask(message).mapTo[Map[String, AnyRef]]
    val res = Await.result(futureResults, duration)
    system.stop(actor)
    res
  }

  def getOrganismsPolySequences(organismList: List[String]):Map[String, List[Neo4jNode]] = {
    val organismActors = organismList.map(name => system.actorOf(Props(OrganismPolySequenceSeeker(name))))
    val futureResults = organismActors.map(ask(_, SeekPolySequenceMessage).mapTo[Map[String, List[Neo4jNode]]])
    val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS))).toMap
    organismActors.foreach(system.stop)
    res
  }

//  def compareSequences() = {
  def compareSequences(mapOfOrganismsAndSequences: Map[String, List[Neo4jNode]])(message: Message) = {

    def loop(
              hd: (String, List[Neo4jNode]),
              tl: Map[String, List[Neo4jNode]],
              matched: Map[String, List[Map[String, List[Neo4jNode]]]]
            ): Map[String, List[Map[String, List[Neo4jNode]]]] = {
      if (tl.nonEmpty) {
        val actors = tl.map(t => system.actorOf(Props(OrganismSequenceMatchSeeker(hd, t))))
        val futureResults = actors.map(ask(_, message).mapTo[List[Map[String, List[Neo4jNode]]]])//.mapTo[List[Neo4jNode]])
        val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS))).toList
        actors.foreach(system.stop)
        loop(tl.head, tl.tail, matched ++ Map(hd._1 -> res))
      }
      else {
        matched
      }
    }

    val mapOfMathced = loop(mapOfOrganismsAndSequences.head, mapOfOrganismsAndSequences.tail, Map())

    def mergeMaps[T <: AnyRef, U >: AnyRef](m: Map[T, List[U]], n: Map[T, List[U]]): Map[T, List[U]] = {
      val keyIntersection = m.keySet & n.keySet
      val r1 = keyIntersection.map(key => key -> (m(key) ++ n(key)))
      //      val r1 = for(key <- keyIntersection) yield key -> (m(key) :: List(n(key)))
      val r2 = m.filterKeys(!keyIntersection.contains(_)) ++ n.filterKeys(!keyIntersection.contains(_))
      r2 ++ r1
    }

//    res.map(org => org)
    val res = mapOfMathced.map(e => e._2.foldLeft(Map[String, List[AnyRef]]())((foldRes, next) => mergeMaps(foldRes, next)))
    res
  }

  def getDataBaseNodes = transaction(graphDataBaseConnection) {
    graphDataBaseConnection
      .findNodes(DynamicLabel.label("DB"))
      .asScala
      .map{db =>
        val name = db.getProperty("name").toString
        name -> DBNode(name, nodeId = db.getId)
      }
      .toMap
  }

  case class UniProtXrefUpdater(xref: (String, Neo4jNode)) extends Actor {

    private def updateSequence() = {
      val client = Client.getServiceFactoryInstance
      val service = client.getUniProtQueryService
      service.start()
      val accession = UniProtQueryBuilder.accession(xref._1)
      val accessionReferences = service.getEntries(accession).asScala
      accessionReferences
        .map(ref => ref
          .getDatabaseCrossReferences
          .asScala)
        .foreach(processCrossReferences)

//      val first = accessionReferences.next()
//      val cref = first.getDatabaseCrossReferences.asScala.toList
//      processCrossReferences(cref)
//      ups.getXrefs()
      service.stop()
    }

    private def processCrossReferences(props: Iterable[DatabaseCrossReference]) = {
      val refs = props.map(e => e.getDatabase.toString -> e.getPrimaryId.toString)
      val dbs = props.map(_.getPrimaryId)
      val desc = props.map(_.getDescription)
      refs.foreach(makeXref)
    }

    private def makeXref(ref: (String, String)): Unit = transaction(graphDataBaseConnection){
      //      todo make relationships to Polypeptides of Sequences
      //      get some more info maybe?
// todo STRINGXRE not found in the DB list
      val dbName = ref._1
      val xrefText = ref._2
      val xrefObject = XRef(xrefText, externalDataBasesCollector(dbName), properties = Map("To_check" -> "True"))
      val poly = Option(xref._2.getSingleRelationship(BiomeDBRelations.evidence, Direction.INCOMING).getStartNode)
      poly match {
        case Some(p) => p.createRelationshipTo(xrefObject.upload(graphDataBaseConnection), BiomeDBRelations.evidence)
        case None => xref._2.createRelationshipTo(xrefObject.upload(graphDataBaseConnection), BiomeDBRelations.evidence)
      }
    }

    def receive = {
      case UpdateUniprot => updateSequence()
      case _ => println(s"Wrong message for UniProt update")
    }

  }

//  private def getOrCreateDBForUniprotUpdate()

  def getUniProtSequences: Map[String, Neo4jNode] = transaction(graphDataBaseConnection) {
    val uniProtXrefs = {
      graphDataBaseConnection.findNode(DynamicLabel.label("DB"), "name", "UniProtKB/Swiss-Prot")
        .getRelationships(BiomeDBRelations.linkTo, Direction.INCOMING)
        .asScala
        .toList
        .map{r =>
          val n = r.getStartNode
          n.getProperty("id").toString -> n
        }.toMap
    }
    uniProtXrefs
  }

  def getUniprotSequenceByID(id: String): Map[String, Neo4jNode] = transaction(graphDataBaseConnection) {
    val node = graphDataBaseConnection.findNode(DynamicLabel.label("AA_Sequence"), "id", id)
    Map(id -> node)
  }

  def updateUniProtSequences(dbNamesFileName: String)(listOfXref: Map[String, Neo4jNode]): Unit = transaction(graphDataBaseConnection) {
    val dbNodes = Source
      .fromFile(dbNamesFileName)
      .getLines()
      .map(name => DBNode(name.trim))
      .map{db =>
        db.upload(graphDataBaseConnection)
        db.name.toUpperCase -> db}
      .toMap
      .par
    externalDataBasesCollector ++=  dbNodes //++ externalDataBasesCollector
    val actors = listOfXref.map(x => system.actorOf(Props(UniProtXrefUpdater(x))))
    actors.foreach(_.ask(UpdateUniprot))

  }

}