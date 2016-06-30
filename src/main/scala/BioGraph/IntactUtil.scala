package BioGraph

import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import psidev.psi.mi.xml.io.impl.PsimiXmlReader253
import psidev.psi.mi.xml.model.{DbReference, ExperimentDescription, ExperimentalInteractor, Interaction, Interactor, EntrySet}
import psidev.psi.mi.xml.{PsimiXmlWriter, PsimiXmlReader, PsimiXmlLightweightReader}
import psidev.psi.mi.xml.xmlindex.impl.PsimiXmlPullParser253

import java.io.File
import utilFunctions.{BiomeDBRelations, TransactionSupport}

import scala.collection.JavaConverters._

/**
  * Created by artem on 16.06.16.
  */
class IntactUtil(dir: String, dataBaseFile: File) extends TransactionSupport {
  val fl = new File(dir)
  val reader = new PsimiXmlReader()
  val readResult = reader.read(fl).getEntries.asScala
  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  var gbs = scala.collection.mutable.Set[String]()
  var mapOfReactants = Map[Int, Reactant]()
  var mapOfExperiments = Map[Int, (String, String, String)]()

  def getInteractors = {
    readResult.map(_.getInteractors.asScala).toList.flatten
  }

  def getInteractions = {
    readResult.map(_.getInteractions.asScala).toList.flatten
  }

  def getExperiments = {
    readResult.map(_.getExperiments.asScala).toList.flatten
  }

  def interactorInfo(interactor: Interactor) = {
    val id = interactor.getId
//    println(id)
    val name = Option(interactor.getNames.getFullName)
    val secondaryName = Option(interactor.getNames.getShortLabel)
//    val geneName = interactor.getNames.getAliases
    val seq = Option(interactor.getSequence)
    val sequence = seq match {
      case Some(s) => s
      case None => ""//println("No sequence: " + id + " " + name)
    }

    def makeMapOfXrefs(refs: List[DbReference], listOfXrefMap: List[Map[String, String]]): List[Map[String, String]] = {
      if (refs.nonEmpty) {
        def makeXRef(ref: DbReference): Map[String, String] = ref.getId.contains(':') match {
          case true => Map(ref.getDb -> ref.getId.split(':')(1))
          case _ =>
            gbs.add(ref.getDb)
            Map(ref.getDb -> ref.getId)
        }
        makeMapOfXrefs(refs.tail, makeXRef(refs.head) :: listOfXrefMap)
      }
      else listOfXrefMap
  }
    val xrefs = interactor.getXref.getAllDbReferences.asScala.toList
//    val x = xrefs.getAllDbReferences.asScala.foreach(makeMapOfRefs)
    var mapOfXrefs = makeMapOfXrefs(xrefs, List())

    (mapOfXrefs, id, name, sequence, secondaryName)
  }

  def interactionInfo(interaction: Interaction) = {
    val name = Option(interaction.getNames.getFullName)
    val secondaryName = Option(interaction.getNames.getShortLabel)
    val id = interaction.getId
    val imexId = interaction.getImexId
    val xref = interaction.getXref.getPrimaryRef.getId
    val participants = interaction.getParticipants.asScala.map(_.getInteractor.getId)
    val experiments = interaction.getExperiments.asScala.map(_.getId).head
    (id, name, xref, participants, experiments, secondaryName, imexId)
  }

  def experimentInfo(experiment: ExperimentDescription) = {
    val expId = experiment.getId
    val expNames = experiment.getNames.getFullName
    val participantDetectionMethod = experiment.getParticipantIdentificationMethod.getNames.getFullName
    val interactionDetectionMethod = experiment.getInteractionDetectionMethod.getNames.getFullName
    (expId, (participantDetectionMethod, interactionDetectionMethod, expNames))
//    List(expId, participantDetectionMethod, interactionDetectionMethod, expNames)
  }

  private def findPolypetidesInteractors(interactors: List[Interactor]) = {
    val parsedInteractors = interactors.map(interactorInfo)
    def findPolyNodeByXRef(xrefMap: List[Map[String, String]]) = {
      def makeQuery(db: String, id: String) = {
        val query = "MATCH (db:DB)<-[:LINK_TO]-" +
          "(:XRef{id:'" + id + "'})<-[:EVIDENCE]-" +
          "(p:Polypeptide) " +
          "WHERE db.name=~'(?i).*" + db + ".*' " +
          "RETURN ID(p)"
        val queryResults = graphDataBaseConnection.execute(query).asScala.toList
        queryResults
      }
      val foundNodes = xrefMap.map(dbAndId => makeQuery(dbAndId.keys.head, dbAndId.values.head))
      foundNodes
    }
    val findExistingNodes = parsedInteractors.map(l => findPolyNodeByXRef(l._1))
    findExistingNodes.flatten.map(_.map(_.asScala))
  }

  def createInteractorNodes(interactors: List[Interactor]): Unit = transaction(graphDataBaseConnection){
    def processOneInteractor(interactor: Interactor, queryResult: List[scala.collection.mutable.Map[String, AnyRef]]): Unit = {
      val info = interactorInfo(interactor)
      val reactantName = info._3 match {
        case Some(name) => name
        case None => info._5 match {
          case Some(secondaryName) => secondaryName
          case None => "unknown"
        }
      }

      val reactant = queryResult.nonEmpty match {
        case true =>
          val reactant = new Reactant(name = reactantName)
          val reactantNode = reactant.upload(graphDataBaseConnection)
          queryResult.head.get("ID(p)") match {
            case Some(polyId) =>
              val polyNode = graphDataBaseConnection.getNodeById(polyId.toString.toLong)
              reactantNode.createRelationshipTo(polyNode, BiomeDBRelations.isA)
            case _ =>
          }
          reactant
        case false =>
          val reactant = new Reactant(name = reactantName, sequence = info._4)
          val reactantNode = reactant.upload(graphDataBaseConnection)
          reactant
      }
      mapOfReactants += (info._2 -> reactant)
    }

    val foundExistingPolyNodes = findPolypetidesInteractors(interactors)
    val zipped = interactors.zip(foundExistingPolyNodes)
    zipped.foreach(z => processOneInteractor(z._1, z._2))
  }

  def createReactionsNodes(interactions: List[Interaction]) = transaction(graphDataBaseConnection) {
    val intactDB = new DBNode("Intact")
    this.getExperiments.map(experimentInfo).foreach(e => mapOfExperiments += (e._1 -> e._2))
    def processOneInteraction(interaction: Interaction): Unit = {
      val info = interactionInfo(interaction)

      val reactionName = info._2 match {
        case Some(name) => name
        case _ => info._6 match {
          case Some(secondaryName) => secondaryName
          case _ => "unknown"
        }
      }

      val listOfReactants = info._4.map(mapOfReactants).toList
      val reaction = new Reaction(reactionName, listOfReactants, info._7, mapOfExperiments(info._5)._3)
      val xref = new XRef(info._3, intactDB).upload(graphDataBaseConnection)
      val interactionNode = reaction.upload(graphDataBaseConnection)
      interactionNode.createRelationshipTo(xref, BiomeDBRelations.evidence)
    }
    interactions.foreach(processOneInteraction)
  }

  private def checkDbName(db: String) = {
    val outputDbName = db.length match {
      case i if i < 3 => db.toUpperCase
      case _ => db.capitalize
    }
    outputDbName
  }
}
