package BioGraph


import org.neo4j.graphdb.factory.GraphDatabaseFactory
import psidev.psi.mi.xml.model.{DbReference, ExperimentDescription, ExperimentalInteractor, Interaction, Interactor, Entry}

import java.io.File
import utilFunctions.{BiomeDBRelations, TransactionSupport}

import scala.collection.JavaConverters._


/**
  * Created by artem on 16.06.16.
  */
class IntactUtil(psiXmlEntries: Iterable[Entry], dataBaseFile: File) extends TransactionSupport {

  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  //  var gbs = scala.collection.mutable.Set[String]()
  var mapOfReactants = Map[Int, Reactant]()
  var mapOfExperiments = Map[Int, ExperimentInfo]()

  val intactDB = new DBNode("Intact")
  val imexDB = new DBNode("Imex")
  var dbDictionary = Map[String, DBNode]()

  def getInteractors = {
    psiXmlEntries.map(_.getInteractors.asScala).toList.flatten
  }

  def getInteractions = {
    psiXmlEntries.map(_.getInteractions.asScala).toList.flatten
  }

  def getExperiments = {
    psiXmlEntries.map(_.getExperiments.asScala).toList.flatten
  }

  def interactorInfo(interactor: Interactor): InteractorInfo = {
    val id = interactor.getId
    val name = Option(interactor.getNames.getFullName)
    val secondaryName = Option(interactor.getNames.getShortLabel)
    val sequence = Option(interactor.getSequence) match {
      case Some(s) => s
      case None => ""
    }
    //    process inchi
    val inchiFind = Option(
      interactor.getAttributes.asScala.toList.filter(
        elem => (elem.getName == "standard inchi") || (elem.getName == "inchi key")
      )
    )
    val inchiMap = inchiFind match {
      case Some(i) => i.map(elem => elem.getName -> inchiWriter(elem.getValue)).toMap[String, String]
      case None => Map[String, String]()
    }

    def makeMapOfXrefs(refs: List[DbReference], listOfXrefMap: Map[String, String]): Map[String, String] = {
      if (refs.nonEmpty) {
        def makeXRef(ref: DbReference): Map[String, String] = ref.getId.contains(':') match {
          case true => Map(ref.getDb -> ref.getId.split(':')(1))
          case _ =>
            //            gbs.add(ref.getDb)
            Map(ref.getDb -> ref.getId)
        }
        makeMapOfXrefs(refs.tail, makeXRef(refs.head) ++ listOfXrefMap)
      }
      else listOfXrefMap
    }
    val xrefs = interactor.getXref.getAllDbReferences.asScala.toList
    val mapOfXrefs = makeMapOfXrefs(xrefs, Map())
    val reactant = new InteractorInfo(id, name, secondaryName, mapOfXrefs, sequence, inchiMap)
    reactant
  }

  def interactionInfo(interaction: Interaction) = {
    val name = Option(interaction.getNames.getFullName)
    val secondaryName = Option(interaction.getNames.getShortLabel)
    val id = interaction.getId
    val imexId = Option(interaction.getImexId)
    val xref = interaction.getXref.getPrimaryRef.getId
    val participants = interaction.getParticipants.asScala.map(_.getInteractor.getId)
    val experiments = interaction.getExperiments.asScala.map(_.getId).head
    val interactionInfo = new InteractionInfo(id, name, secondaryName, xref, participants, experiments, imexId)
    interactionInfo
  }

  def experimentInfo(experiment: ExperimentDescription) = {
    val expId = experiment.getId
    val fullName = experiment.getNames.getFullName
    val participantDetectionMethod = experiment.getParticipantIdentificationMethod.getNames.getFullName
    val interactionDetectionMethod = experiment.getInteractionDetectionMethod.getNames.getFullName
    val experimentInfo = new ExperimentInfo(expId, participantDetectionMethod, interactionDetectionMethod, fullName)
    experimentInfo
  }

  private def findPolypetidesInteractors(parsedInteractors: List[InteractorInfo]) = {
    def findPolyNodeByXRef(xrefMap: Map[String, String]) = {
      def makeQuery(db: String, id: String) = {
        val query = "MATCH (db:DB)<-[:LINK_TO]-" +
          "(:XRef{id:'" + id + "'})<-[:EVIDENCE]-" +
          "(p:Polypeptide) " +
          "WHERE db.name=~'(?i).*" + db + ".*' " +
          "RETURN ID(p)"
        val queryResults = graphDataBaseConnection.execute(query).asScala.toList
        queryResults
      }
      val foundNodes = xrefMap.map(elem => makeQuery(elem._1, elem._2))
      foundNodes
    }
    val findExistingNodes = parsedInteractors.map(i => findPolyNodeByXRef(i.getXrefs))
    findExistingNodes.flatten.map(_.map(_.asScala))
  }

  def createInteractorNodes(interactors: List[Interactor]): Unit = transaction(graphDataBaseConnection) {
    def processOneInteractor(parsedInteractor: InteractorInfo, queryResult: List[scala.collection.mutable.Map[String, AnyRef]]): Unit = {
      //      Name
      val reactantName = parsedInteractor.getName

      //      inchi
      val inchiMap = parsedInteractor.getInchi

      //      xrefs
//      val xrefObjects = parsedInteractor.getXrefs.map(elem => new XRef(elem._2, new DBNode(elem._1)))
      val xrefObjects = parsedInteractor.getXrefs.map(elem => makeXrefNode(elem))
      val xrefNodes = xrefObjects.map(_.upload(graphDataBaseConnection))

      //      Try to match a polypeptide in DB to a reactant
      val reactant = queryResult.nonEmpty match {
        case true =>
          val reactant = new Reactant(name = reactantName)
          val reactantNode = reactant.upload(graphDataBaseConnection)
          xrefNodes.foreach(reactantNode.createRelationshipTo(_, BiomeDBRelations.evidence))
          queryResult.head.get("ID(p)") match {
            case Some(polyId) =>
              val polyNode = graphDataBaseConnection.getNodeById(polyId.toString.toLong)
              reactantNode.createRelationshipTo(polyNode, BiomeDBRelations.isA)
            case _ => println("Something went wrong while matching poly to Reactant " + queryResult)
          }
          reactant
        case false =>
          val reactant = new Reactant(
            name = reactantName,
            sequence = parsedInteractor.getSequence,
            inchi = inchiMap,
            toCheck = true)
          val reactantNode = reactant.upload(graphDataBaseConnection)
          xrefNodes.foreach(reactantNode.createRelationshipTo(_, BiomeDBRelations.evidence))
          reactant
      }
      mapOfReactants += (parsedInteractor.getId -> reactant)

    }
    val parsedInteractors = interactors.map(interactorInfo)
    val foundExistingPolyNodes = findPolypetidesInteractors(parsedInteractors)
    val zipped = parsedInteractors.zip(foundExistingPolyNodes)
    zipped.foreach(z => processOneInteractor(z._1, z._2))
  }

  def createReactionsNodes(interactions: List[Interaction]): Unit = transaction(graphDataBaseConnection) {
    //    make a Map of experiments by their Intact id
    this.getExperiments.map(experimentInfo).foreach(elem => mapOfExperiments += (elem.getId -> elem))
    def processOneInteraction(interaction: Interaction): Unit = {
      val info = interactionInfo(interaction)
      //      name
      val reactionName = info.getName match {
        case Some(name) => name
        case _ => info.getSecondaryName match {
          case Some(secondaryName) => secondaryName
          case _ => "unknown"
        }
      }
      //      xrefs
      val listOfXrefs = info.getImexId match {
        case Some(id) => List(new XRef(info.getIntactId, intactDB), new XRef(id, imexDB))
        case None => List(new XRef(info.getIntactId, intactDB))
      }
      //      reactants
      val listOfReactants = info.getParticipants.map(mapOfReactants).toList
      val reaction = new Reaction(
        reactionName,
        listOfReactants,
        listOfXrefs,
        mapOfExperiments(info.getExperiments).getFullName
      )
      reaction.upload(graphDataBaseConnection)
    }
    interactions.foreach(processOneInteraction)
  }

  private def makeXrefNode(dbXrefPair: (String, String)) = {
    val dbName = dbXrefPair._1.capitalize
    val dbObject = dbDictionary.contains(dbName) match {
      case true => dbDictionary(dbName)
      case false =>
        val newDB = new DBNode(dbName)
        dbDictionary += (dbName -> newDB)
        newDB
    }
    new XRef(dbXrefPair._2, dbObject)
  }

  private def inchiWriter(inchiString: String): String = {
    if (inchiString.contains('=')) inchiString.split('=')(1)
    else inchiString
  }
}

abstract class PPIInfo(id: Int) {

  def getId = id
}

class InteractorInfo(
                      id: Int,
                      name: Option[String],
                      secondaryName: Option[String],
                      xrefs: Map[String, String],
                      seq: String,
                      inchiMap: Map[String, String])
  extends PPIInfo(id) {

  def getName = name match {
    case Some(n) => n
    case None => secondaryName match {
      case Some(sn) => sn
      case None => "unknown"
    }
  }

  def getInchi = inchiMap

  def getXrefs = xrefs

  def getSequence = seq
}

class InteractionInfo(
                       id: Int,
                       name: Option[String],
                       secondaryName: Option[String],
                       intactId: String,
                       participants: Iterable[Int],
                       experiments: Int,
                       imexId: Option[String]
                    )
  extends PPIInfo(id) {

  def getName = name

  def getSecondaryName = secondaryName

  def getIntactId = intactId

  def getParticipants = participants

  def getExperiments = experiments

  def getImexId = imexId

}

class ExperimentInfo(
                      id: Int,
                      participantDetectionMethod: String,
                      interactionDetectionMethod: String,
                      fullName: String
                    )
  extends PPIInfo(id) {

  def getParticipantDetectionMethod = participantDetectionMethod

  def getInteractionDetectionMethod = interactionDetectionMethod

  def getFullName = fullName

}