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
import org.biojavax.bio.db.ncbi.GenbankRichSequenceDB
import uk.ac.ebi.kraken.interfaces.uniprot.comments.DomainComment
import uk.ac.ebi.kraken.interfaces.uniprot.features.{DomainFeature, FeatureType}
import uk.ac.ebi.kraken.model.uniprot.features.DomainFeatureImpl

case class ActorQueriesUtil(pathToDataBase: String, system: ActorSystem)(implicit timeout: Timeout) extends WorkWithGraph(pathToDataBase) {

  type Neo4jNode = org.neo4j.graphdb.Node

  trait Message

  case object SeekSequenceMessage extends Message
  case object FindSimilarPolyMessage extends Message
  case object MatchPolySequencesDifferenceMessage extends Message
  case object CypherQueryMessage extends Message
  case object UpdateUniprotMessage extends Message
  case object FindIdentical extends Message

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

  case class OrganismSequenceSeeker(organismName: String, sequenceType: String = "AA_Sequence") extends Actor {

    def seekPolySequences: Map[String, List[AnyRef]] = transaction(graphDataBaseConnection) {
      val query = sequenceType match{
        case "AA_Sequence" => s"MATCH (:Organism{name:'$organismName'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(seq:AA_Sequence)" +
      "RETURN COLLECT(seq)"
        case "DNA_Sequence" => s"MATCH (:Organism{name:'$organismName'})<-[:PART_OF]-(:Gene)-[:IS_A]->(seq:DNA_Sequence)" +
          "RETURN COLLECT(seq)"
      }
      val sequencesActor = system.actorOf(Props(CypherQueryExecutor(query)))
      val sequencesCollection = getCollectionOfNodes(sequencesActor, CypherQueryMessage)()
      val sequences = getCollectionResults(sequencesCollection).head
      Map(organismName -> sequences)
    }

    def receive = {
      case SeekSequenceMessage => sender ! seekPolySequences
      case _ => sender ! Map[String, List[AnyRef]]()
    }
  }

  case class OrganismSequenceMatchSeeker(fromOrganism: (String, List[Neo4jNode]), toOrganism: (String, List[Neo4jNode])) extends Actor {

    def findSimilar(): Map[String, List[List[Neo4jNode]]] = transaction(graphDataBaseConnection) {
      val res = fromOrganism._2.flatMap {
        from => toOrganism._2.map {
          to => utilFunctions.utilFunctionsObject.matchRelationExistenceWithoutDirection(from, to)
        }
      }.filter(_.nonEmpty)
      Map(s"${fromOrganism._1} -> ${toOrganism._1}" -> res)
    }

    def findIdentical(): Map[String, List[Neo4jNode]] = transaction(graphDataBaseConnection) {
      val filtered = fromOrganism._2.filter(toOrganism._2.contains(_))
      Map(s"${fromOrganism._1} -> ${toOrganism._1}" -> filtered)
    }

    def findPolyDifference(): Map[String, List[Neo4jNode]] = transaction(graphDataBaseConnection) {
      val queryForSimilarity = s"MATCH (:Organism{name:'${fromOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s1:Sequence)-[:SIMILAR]-(s2:Sequence)<-[:IS_A]-(:Polypeptide)-[:PART_OF]->(:Organism{name:'${toOrganism._1}'})" +
        s" RETURN COLLECT(DISTINCT s1), COLLECT(DISTINCT s2)"
      val queryForIdenticalSequences = s"MATCH (:Organism{name:'${fromOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s1:Sequence)<-[:IS_A]-(:Polypeptide)-[:PART_OF]->(:Organism{name:'${toOrganism._1}'})" +
        s" RETURN COLLECT(DISTINCT s1)"

//      val queryFromOrganismSequences = s"MATCH (:Organism{name:'${fromOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s1:Sequence)" +
//        s" RETURN COLLECT(DISTINCT s1)"
//      val queryToOrganismSequences = s"MATCH (:Organism{name:'${toOrganism._1}'})<-[:PART_OF]-(:Polypeptide)-[:IS_A]->(s2:Sequence)" +
//        s" RETURN COLLECT(DISTINCT s2)"

      val organismsSequences = getOrganismsPolySequences(List(fromOrganism._1, toOrganism._1))
      val fromOrganismSequences = organismsSequences(fromOrganism._1).toSet
      val toOrganismSequences = organismsSequences(toOrganism._1).toSet


      val similarSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryForSimilarity)))
      val identicalSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryForIdenticalSequences)))
//      val fromSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryFromOrganismSequences)))
//      val toSequencesActor = system.actorOf(Props(CypherQueryExecutor(queryToOrganismSequences)))

      val similarSequencesRes = getCollectionOfNodes(similarSequencesActor, CypherQueryMessage)()
      val similarSequences = getCollectionResults(similarSequencesRes)
      val fromSimilarSequences = similarSequences.head.toSet
      val toSimilarSequences = similarSequences.last.toSet

      val identicalCollectionSequences = getCollectionOfNodes(identicalSequencesActor, CypherQueryMessage)()
      val identicalSequences = getCollectionResults(identicalCollectionSequences).head.toSet

//      val fromCollectionSequences = getCollectionOfNodes(fromSequencesActor, CypherQueryMessage)()
//      val fromSequences = getCollectionResults(fromCollectionSequences).head.toSet
//
//      val toCollectionSequences = getCollectionOfNodes(toSequencesActor, CypherQueryMessage)()
//      val toSequences = getCollectionResults(toCollectionSequences).head.toSet

      val similarSequencesIntersection = fromSimilarSequences.union(toSimilarSequences).union(identicalSequences)
      val fromDifference = fromOrganismSequences.diff(similarSequencesIntersection).diff(identicalSequences)
      val toDifference = toOrganismSequences.diff(similarSequencesIntersection).diff(identicalSequences)
      val res = Map(fromOrganism._1 -> fromDifference.toList, toOrganism._1 -> toDifference.toList)
      res
    }

    def receive = {
      case FindSimilarPolyMessage => sender ! findSimilar()
      case MatchPolySequencesDifferenceMessage => sender ! findPolyDifference()
      case FindIdentical => sender ! findIdentical()
      case _ => sender ! Map[String, List[AnyRef]]()
    }
  }

  private case class CypherQueryExecutor(query: String) extends Actor{
    def receive = {
      case CypherQueryMessage => sender ! {
        val res = graphDataBaseConnection.execute(query).asScala//.toList
        res.next.asScala.toMap
      }
      case _ => sender ! Map()
    }
  }

  private def getCollectionOfNodes(actor: ActorRef, message: Message)(duration: Duration = Duration(100, TimeUnit.SECONDS)) = {
    val futureResults = actor.ask(message).mapTo[Map[String, AnyRef]]
    val res = Await.result(futureResults, duration)
    system.stop(actor)
    res
  }

  private def getCollectionResults(queryResult: Map[String, AnyRef]): Iterable[List[Neo4jNode]] = {
    val res = queryResult
      .values
      .map(_
        .asInstanceOf[SeqWrapper[Neo4jNode]]
        .asScala
        .toList)
    res
  }

  def getOrganismsPolySequences(organismList: List[String], sequenceType: String = "AA_Sequence"): Map[String, List[Neo4jNode]] = {
    val organismActors = organismList.map(name => system.actorOf(Props(OrganismSequenceSeeker(name, sequenceType))))
    val futureResults = organismActors.map(ask(_, SeekSequenceMessage).mapTo[Map[String, List[Neo4jNode]]])
    val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS))).toMap
    organismActors.foreach(system.stop)
    res
  }

  def compareSequences(mapOfOrganismsAndSequences: Map[String, List[Neo4jNode]]): Map[String, List[Map[String, List[Neo4jNode]]]] = {

    def loop(
              hd: (String, List[Neo4jNode]),
              tl: Map[String, List[Neo4jNode]],
              matched: Map[String, List[Map[String, List[Neo4jNode]]]]
            ): Map[String, List[Map[String, List[Neo4jNode]]]] = {
      if (tl.nonEmpty) {
        val actors = tl.map(t => system.actorOf(Props(OrganismSequenceMatchSeeker(hd, t))))
        val futureResults = actors.map(ask(_, MatchPolySequencesDifferenceMessage).mapTo[Map[String, List[Neo4jNode]]])//.mapTo[List[Neo4jNode]])
        val res = futureResults.map(Await.result(_, Duration(100, TimeUnit.SECONDS))).toList

        actors.foreach(system.stop)
        loop(tl.head, tl.tail, matched ++ Map(hd._1 -> res))
      }
      else {
        matched
      }
    }

    val mapOfMatched = loop(mapOfOrganismsAndSequences.head, mapOfOrganismsAndSequences.tail, Map())

    def mergeMaps[T <: AnyRef, U >: AnyRef](m: Map[T, List[U]], n: Map[T, List[U]]): Map[T, List[U]] = {
      val keyIntersection = m.keySet & n.keySet
      val r1 = keyIntersection.map(key => key -> (m(key) ++ n(key)))//.toMap
      //      val r1 = for(key <- keyIntersection) yield key -> (m(key) :: List(n(key)))
      val r2 = m.filterKeys(!keyIntersection.contains(_)) ++ n.filterKeys(!keyIntersection.contains(_))
      r2 ++ r1
    }


//    val res = mapOfMatched
//      .map(e => e._2.foldLeft(Map[String, List[AnyRef]]())((foldRes, next) => mergeMaps(foldRes, next)))
//    res
    mapOfMatched
  }

  def findSimilarSequences(mapOfOrganismsAndSequences: Map[String, List[Neo4jNode]]): Map[String, List[List[Neo4jNode]]] = {
    def loop(
              hd: (String, List[Neo4jNode]),
              tl: Map[String, List[Neo4jNode]],
              matched: Map[String, List[List[Neo4jNode]]]
            ): Map[String, List[List[Neo4jNode]]]  = {
      if (tl.nonEmpty) {
        val actors = tl.map(t => system.actorOf(Props(OrganismSequenceMatchSeeker(hd, t))))
        val futureResults = actors.map(ask(_, FindSimilarPolyMessage).mapTo[Map[String, List[List[Neo4jNode]]]])//.mapTo[List[Neo4jNode]])
        val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS)))
        actors.foreach(system.stop)

        loop(tl.head, tl.tail, matched ++ res)
      }
      else {
        matched
      }
    }

    loop(mapOfOrganismsAndSequences.head, mapOfOrganismsAndSequences.tail, Map())
  }

  def findIdentical(mapOfOrganismsAndSequences: Map[String, List[Neo4jNode]]): Map[String, List[Neo4jNode]] = {
    def loop(
              hd: (String, List[Neo4jNode]),
              tl: Map[String, List[Neo4jNode]],
              matched: Map[String, List[Neo4jNode]]
            ): Map[String, List[Neo4jNode]] = {
      if (tl.nonEmpty) {
        val actors = tl.map(t => system.actorOf(Props(OrganismSequenceMatchSeeker(hd, t))))
        val futureResults = actors.map(ask(_, FindIdentical).mapTo[Map[String, List[Neo4jNode]]])//.mapTo[List[Neo4jNode]])
        val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS))).toMap
        actors.foreach(system.stop)

        loop(tl.head, tl.tail, matched ++ res)
      }
      else matched
    }
    loop(mapOfOrganismsAndSequences.head, mapOfOrganismsAndSequences.tail, Map())
  }

  def getDataBaseNodes: Map[String, DBNode] = transaction(graphDataBaseConnection) {
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

    private def updateSequence(): Unit = {
      val client = Client.getServiceFactoryInstance
      val service = client.getUniProtQueryService
      service.start()
      val accession = UniProtQueryBuilder.accession(xref._1)
      val qs = accession.getQueryString
      val features = service.getFeatures(accession).asScala.toList
//      val comments = service.getComments(accession).asScala.toList.flatMap(_.getComponent.asScala)
      val accessionReferences = service.getEntries(accession).asScala.toList
      val domains = accessionReferences.flatMap(ref => ref.getFeatures(FeatureType.DOMAIN).asScala).toIterable
//        .flatMap[DomainFeatureImpl](ref => ref.getFeatures(FeatureType.DOMAIN).asScala)
//        .toIterable
      domains.foreach(processDomain)

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

    private def processCrossReferences(props: Iterable[DatabaseCrossReference]): Unit = transaction(graphDataBaseConnection){
      val refs = props.map(e => e.getDatabase.toString -> e.getPrimaryId.toString)
      val dbs = props.map(_.getPrimaryId)
      val desc = props.map(_.getDescription)
      refs.foreach(makeXref)
    }

    private def processDomain(domain: DomainFeatureImpl): Unit = transaction(graphDataBaseConnection){
//      todo double relationships
      val features = domain.getFeatureDescription
      val location = domain.getFeatureLocation
      val domainObject = Domain(features.getValue)
//      connect Domain to Sequence or Poly?
      val poly = Option(xref._2.getSingleRelationship(BiomeDBRelations.evidence, Direction.INCOMING).getStartNode)
      val rel = poly match {
        case Some(p) =>
          domainObject.upload(graphDataBaseConnection).createRelationshipTo(p, BiomeDBRelations.partOf)
        case None =>
          domainObject.upload(graphDataBaseConnection).createRelationshipTo(xref._2, BiomeDBRelations.partOf)
      }
      rel.setProperty("start", location.getStart)
      rel.setProperty("end", location.getEnd)
    }

    private def makeXref(ref: (String, String)): Unit = transaction(graphDataBaseConnection){
      //      todo make relationships to Polypeptides of Sequences
      //      get some more info maybe?
      val dbName = ref._1
      val xrefText = ref._2
      val findDB =
        if (externalDataBasesCollector.contains(dbName)) externalDataBasesCollector(dbName)
        else {
          val db = DBNode(dbName)
          db.upload(graphDataBaseConnection)
          externalDataBasesCollector ++= Map(dbName -> db)
          db
        }

      val xrefObject = XRef(xrefText, findDB, properties = Map("To_check" -> "True")).upload(graphDataBaseConnection)
      val poly = Option(xref._2.getSingleRelationship(BiomeDBRelations.evidence, Direction.INCOMING).getStartNode)
      poly match {
        case Some(p) => p.createRelationshipTo(xrefObject, BiomeDBRelations.evidence)
        case None => xref._2.createRelationshipTo(xrefObject, BiomeDBRelations.evidence)
      }
    }

    def receive = {
      case UpdateUniprotMessage => updateSequence()
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

  private def getOrCreateDBNodes(dbNamesFileName: String): Unit = transaction(graphDataBaseConnection) {
    val dbNodes = Source
      .fromFile(dbNamesFileName)
      .getLines()
      .map(name => DBNode(name.trim))
      .map{db =>
//        db.upload(graphDataBaseConnection)
        db.name.toUpperCase -> db}
      .toMap

    dbNodes.foreach(_._2.upload(graphDataBaseConnection))
    externalDataBasesCollector ++=  dbNodes.par
  }

  def updateUniProtSequences
  (dbNamesFileName: String)(listOfXref: Map[String, Neo4jNode]): Unit = {
    getOrCreateDBNodes(dbNamesFileName)
    val actors = listOfXref.map(x => system.actorOf(Props(UniProtXrefUpdater(x))))
    actors.foreach(_.ask(UpdateUniprotMessage))
    val futureResults = actors.map(ask(_, UpdateUniprotMessage))
    futureResults.foreach(Await.result(_, Duration(100, TimeUnit.SECONDS)))
//    val futureResults = actors.map(ask(_, FindSimilarPolyMessage).mapTo[Map[String, List[List[Neo4jNode]]]])//.mapTo[List[Neo4jNode]])
//    val res = futureResults.flatMap(Await.result(_, Duration(100, TimeUnit.SECONDS)))

  }

}