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
  var mapOfReactants = Map[Int, Long]()

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
//    val geneName = interactor.getNames.getAliases
    val seq = Option(interactor.getSequence)
    val sequence = seq match {
      case Some(s) => s
      case None => //println("No sequence: " + id + " " + name)
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

    (mapOfXrefs, id, name, seq)
  }

  def interactionInfo(interaction: Interaction) = {
    val name = Option(interaction.getNames.getFullName)
    val secondaryName = Option(interaction.getNames.getShortLabel)
    val id = interaction.getId
    val xref = interaction.getXref.getPrimaryRef.getId
    val participants = interaction.getParticipants.asScala.map(_.getInteractor.getId)
    val experiments = interaction.getExperiments.asScala.map(_.getId).head
    (id, name, xref, participants, experiments, secondaryName)
  }

  def experimentInfo(experiment: ExperimentDescription) = {
    val expId = experiment.getId
    val expNames = experiment.getNames.getFullName
    val participantDetectionMethod = experiment.getParticipantIdentificationMethod.getNames.getFullName
    val interactionDetectionMethod = experiment.getInteractionDetectionMethod.getNames.getFullName
//    val expId = experiment.getExperimentRefs
//    val expNames = experiment.getExperiments.asScala.map(_.getNames).head
    List(expId, participantDetectionMethod, interactionDetectionMethod, expNames)
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
      val reactantNode = graphDataBaseConnection.createNode(DynamicLabel.label("Reactant"))
      reactantNode.setProperty("Intact id", info._2)
      info._3 match {
        case Some(name) => reactantNode.setProperty("name", name)
        case _ =>
      }
      mapOfReactants += (info._2 -> reactantNode.getId)
      if (queryResult.nonEmpty) {
        queryResult.head.get("ID(p)") match {
          case Some(polyId) =>
            val polyNode = graphDataBaseConnection.getNodeById(polyId.toString.toLong)
            reactantNode.createRelationshipTo(polyNode, BiomeDBRelations.isA)
          case _ =>
        }
      }
      else {
        reactantNode.addLabel(DynamicLabel.label("To_check"))
        info._4 match {
          case Some(seq) => reactantNode.setProperty("seq", seq)
          case _ =>
        }
      }
    }

    val foundExistingPolyNodes = findPolypetidesInteractors(interactors)
    val zipped = interactors.zip(foundExistingPolyNodes)
    zipped.foreach(z => processOneInteractor(z._1, z._2))
  }

  def createReactionsNodes(interactions: List[Interaction]) = transaction(graphDataBaseConnection) {
    val intactDB = new DBNode("Intact")
    def processOneInteraction(interaction: Interaction): Unit = {
      val info = interactionInfo(interaction)
      val interactionNode = graphDataBaseConnection.createNode(DynamicLabel.label("Reaction"))

      info._2 match {
        case Some(name) => interactionNode.setProperty("name", name)
        case _ => info._6 match {
          case Some(secondaryName) => interactionNode.setProperty("name", secondaryName)
          case _ =>
        }
      }

      def createRelationshipToInteractor(interactorId: Int): Unit = {
        val interactorNode = graphDataBaseConnection.getNodeById(mapOfReactants(interactorId))
        interactorNode.createRelationshipTo(interactionNode, BiomeDBRelations.participates_in)
      }
      info._4.foreach(createRelationshipToInteractor)

      val xref = new XRef(info._3, intactDB).upload(graphDataBaseConnection)
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
