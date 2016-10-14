package BioGraph

import java.io.File

import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml._
import org.sbml.jsbml.ext.fbc.{FBCModelPlugin, GeneProduct}
import utilFunctions.TransactionSupport
import org.neo4j.graphdb.{DynamicLabel, Node}
import utilFunctions.utilFunctionsObject._
import utilFunctions.BiomeDBRelations

import scala.collection.JavaConverters._
import scala.collection.immutable.Map

/**
  * Created by artem on 14.07.16.
  */
class JSBMLUtil(dataBaseFile: File) extends TransactionSupport {
  val logger = LogManager.getLogger(this.getClass.getName)
  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  val reader = new SBMLReader()
  var compartmentNodes = Map[String, Compartment]()
//  get dictionary of ChEBI XRefs
  val chebiNodeId = findNode(graphDataBaseConnection, "DB", "name", "ChEBI").getId
  val chebi = DBNode(name = "ChEBI", nodeId = chebiNodeId)
  var totalChebiCompoundCollector: Map[String, Compound] =
    getNodesDict(graphDataBaseConnection)(getCompoundPropertiesByXRefs, "XRef")(_.getProperty("id").toString.contains("CHEBI:"))
  totalChebiCompoundCollector = totalChebiCompoundCollector.filter(elem => elem._1.contains("CHEBI"))

  def processSBMLFile(fileName: File): Model = transaction(graphDataBaseConnection){
//    get one model
    val readResult = reader.readSBML(fileName)
    val parsedModel = readResult.getModel
    val compartments = parsedModel.getListOfCompartments.asScala.toList
    compartmentNodes = compartments.map(elem => elem.getId -> Compartment(elem.getName)).toMap
    parsedModel
  }

  def uploadModels(parsedModel: Model) = transaction(graphDataBaseConnection) {
    val reactions = parsedModel.getListOfReactions.asScala.toList

    def makeCompoundObject(specie: Species, specieName: String): List[Compound] = transaction(graphDataBaseConnection) {
      val chebiXRefs = specie.getCVTerms.asScala.head.getResources.asScala.toList.filter(_.contains("CHEBI:"))
      def getOrCreateCompoundNode(chebiXRef: String): Compound = {
        val shortXRef = chebiXRef.split("/").last
        if (totalChebiCompoundCollector.contains(shortXRef)) totalChebiCompoundCollector(shortXRef)
        else {
          logger.warn("No compound was found by XRef id:" + chebiXRef)
          val createdCompound = Compound(specieName)
          val createdXRef = XRef(shortXRef, chebi)
          val createdCompoundNode = createdCompound.upload(graphDataBaseConnection)
          val createdXRefNode = createdXRef.upload(graphDataBaseConnection)
          createdCompoundNode.createRelationshipTo(createdXRefNode, BiomeDBRelations.evidence)
          totalChebiCompoundCollector ++= Map(shortXRef -> createdCompound)
          createdCompound
        }
      }

      val compounds = chebiXRefs.map(getOrCreateCompoundNode)
      compounds
    }

    def makeReactantObject(speciesReference: SpeciesReference): Map[Reactant, List[Compound]] = {
      val specie = parsedModel.getSpecies(speciesReference.getSpecies)
      val specieName = specie.getName
      val compartment = specie.getCompartment
      val stoi = speciesReference.getStoichiometry

      val compounds = makeCompoundObject(specie, specieName)
//      make return as reactant and several compounds
      val reactant = Reactant(
        name = specieName,
        stoichiometry = Some(stoi),
        compartment = Some(compartmentNodes(compartment))
      )
      Map(reactant -> compounds)
    }

    def reactantAndCompoundUploader(reactantCompoundPair: Tuple2[Reactant, Compound]) = {
//      make upload for several compounds - foreach
      val reactantNode = reactantCompoundPair._1.upload(graphDataBaseConnection)
      val compoundNode = reactantCompoundPair._2.upload(graphDataBaseConnection)
      val isARel = reactantNode.createRelationshipTo(compoundNode, BiomeDBRelations.isA)
    }

    def makeReactionObject(reaction: org.sbml.jsbml.Reaction): Reaction = {
      val reactants = reaction.getListOfReactants.asScala.toList
      val reactantObjects = reactants.map(makeReactantObject)
//      reactantObjects.foreach(_.upload(graphDataBaseConnection))
      val listOfReactants = reactantObjects.map(uploadReactantsAndCompounds)
      Reaction(reaction.getName, listOfReactants)
    }

    def uploadReactantsAndCompounds(reactantsAndCompounds: Map[Reactant, List[Compound]]): Reactant = {
      val reactant = reactantsAndCompounds.keys.head
      val reactantNode = reactant.upload(graphDataBaseConnection)
      val compoundNodes = reactantsAndCompounds(reactant).map(_.upload(graphDataBaseConnection))
      compoundNodes.foreach(elem => reactantNode.createRelationshipTo(elem, BiomeDBRelations.isA))
      reactant
    }

    def makeGeneProductObject(geneProduct: GeneProduct): Reactant = {
      val fbc = new FBCModelPlugin(parsedModel)
      val geneProducts = fbc.getListOfGeneProducts
      geneProducts.asScala.map(gp => gp.getName)
      Reactant("")
    }

    val reactionObjects = reactions.map(makeReactionObject)
    reactionObjects.map(_.upload(graphDataBaseConnection))
  }

}

