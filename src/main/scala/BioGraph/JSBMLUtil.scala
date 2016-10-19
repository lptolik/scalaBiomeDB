package BioGraph

import java.io.File

import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml._
import org.sbml.jsbml.ext.fbc.{FBCModelPlugin, FBCSpeciesPlugin, GeneProduct}
import utilFunctions.TransactionSupport
import org.neo4j.graphdb.{Direction, DynamicLabel, Node}
import utilFunctions.utilFunctionsObject._
import utilFunctions.BiomeDBRelations

import scala.collection.JavaConverters._
import scala.collection.immutable.Map
import scala.util.{Failure, Success, Try}

/**
  * Created by artem on 14.07.16.
  */
class JSBMLUtil(dataBaseFile: File) extends TransactionSupport {
  val logger = LogManager.getLogger(this.getClass.getName)
  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  val reader = new SBMLReader()
  var compartmentNodes = Map[String, Compartment]()
//  get dictionary of ChEBI and Reactome XRefs
  val chebiInfo = getDataBasesNodes("CheEBI")
  val reactomeInfo = getDataBasesNodes("Reactome")
  val chebi = chebiInfo._1
//  val chebiNodeId = chebiInfo._2
  val reactome = reactomeInfo._1
//  val reactomeNodeId = reactomeInfo._2
  val totalChebiCompoundCollector: Map[String, Compound] =
    getNodesDict(graphDataBaseConnection)(getCompoundPropertiesByXRefs, "XRef")(_.getProperty("id").toString.contains("CHEBI:"))
      .filter(elem => elem._1.contains("CHEBI"))
  val totalReactomeCompoundCollector: Map[String, Compound] =
    getNodesDict(graphDataBaseConnection)(getCompoundPropertiesByXRefs, "XRef")(_.getProperty("id").toString.contains("reactome"))
      .filter(elem => elem._1.contains("reactome"))
  var totalCompoundCollector = totalChebiCompoundCollector ++ totalReactomeCompoundCollector
  var reactantCollector: Map[String, Reactant] = Map()//getNodesDict(graphDataBaseConnection)(getReactantProperties, "Reactant")()
  var geneProductCollector: Map[String, org.neo4j.graphdb.Node] = Map()

  def getDataBasesNodes(dbName: String): (DBNode, Long) = transaction(graphDataBaseConnection){
    val db = DBNode(dbName)
    val dbNodeId = db.upload(graphDataBaseConnection).getId
    (db, dbNodeId)
  }

  def processSBMLFile(fileName: File): Model = transaction(graphDataBaseConnection){
//    get one model
    val readResult = reader.readSBML(fileName)
    val parsedModel = readResult.getModel
    val compartments = parsedModel.getListOfCompartments.asScala.toList
    compartmentNodes = compartments.map(elem => elem.getId -> Compartment(elem.getName)).toMap
    parsedModel
  }

  def uploadModels(parsedModel: Model) = transaction(graphDataBaseConnection) {
    val fbcModel = parsedModel.getModel.getPlugin("fbc")//.asInstanceOf[FBCModelPlugin]
    fbcModel match {
      case model: FBCModelPlugin =>
        val listOfGeneProducts = model.getListOfGeneProducts.asScala.toList
        geneProductCollector ++= listOfGeneProducts.map(getPolypeptideByLocusTag)
      case _ =>
    }


    val reactions = parsedModel.getListOfReactions.asScala.toList
//    val fbc = new FBCModelPlugin(parsedModel)
//    val nu = fbc.getNumGeneProducts
//    val geneProducts = fbc.getListOfGeneProducts.asScala.toList
//    val res = geneProducts.map(getPolypeptideByLocusTag)
//    res
//    geneProducts.map(makeGeneProductObject)

    def makeCompoundObject(specie: Species, specieName: String): List[Compound] = transaction(graphDataBaseConnection) {
      val chebiXRefs = specie.getCVTerms.asScala.head.getResources.asScala.toList.filter(_.contains("CHEBI:"))
      val reactomeXRefs = specie.getCVTerms.asScala.head.getResources.asScala.toList.filter(_.contains("reactome"))
      def getOrCreateCompoundNode(xrefName: String, db: DBNode): Compound = {
        val shortXRefName = xrefName.split("/").last
        if (totalCompoundCollector.contains(shortXRefName)) totalCompoundCollector(shortXRefName)
        else {
          logger.warn("No compound was found by XRef id:" + xrefName)
          val createdCompound = Compound(specieName)
          val createdXRef = XRef(shortXRefName, chebi)
          val createdCompoundNode = createdCompound.upload(graphDataBaseConnection)
          val createdXRefNode = createdXRef.upload(graphDataBaseConnection)
          createdCompoundNode.createRelationshipTo(createdXRefNode, BiomeDBRelations.evidence)
          totalCompoundCollector ++= Map(shortXRefName -> createdCompound)
          createdCompound
        }
      }

      val compounds = chebiXRefs.map(getOrCreateCompoundNode(_, chebi)) ++ reactomeXRefs.map(getOrCreateCompoundNode(_, reactome))
      compounds.distinct
    }

    def makeReactantObject(speciesReference: SpeciesReference, productFlag:Boolean = false): Reactant = {
      val specie = parsedModel.getSpecies(speciesReference.getSpecies)
      val specieName = specie.getName
      val compartment = specie.getCompartment
      val stoi = speciesReference.getStoichiometry
      val metaId = specie.getMetaId
      val specieFBC = specie.getPlugin("fbc")//.asInstanceOf[FBCSpeciesPlugin]
      val stoiFactor = productFlag match {
        case true => 1
        case false => -1
      }

      val compounds = makeCompoundObject(specie, specieName)
      val reactant = reactantCollector.contains(metaId) match {
        case true => reactantCollector(metaId)
        case false =>
          //      make return as reactant and several compounds
          val formula = specieFBC match {
            case fbc:FBCSpeciesPlugin => Some(fbc.getChemicalFormula)
            case _ => None
          }
          val charge = specieFBC match {
            case fbc:FBCSpeciesPlugin => Some(fbc.getCharge)
            case _ => None
          }
          val r = Reactant(
            name = specieName,
            stoichiometry = Some(stoiFactor * stoi),
            compartment = Some(compartmentNodes(compartment)),
            compounds = compounds,
            formula = formula,
            charge = charge,
            properties = Map(
              "metaId" -> metaId,
              "sboTerm" -> specie.getSBOTerm
            )
          )
          reactantCollector ++= Map(metaId -> r)
          r
      }
      reactant
    }

    def makeReactionObject(reaction: org.sbml.jsbml.Reaction): Reaction = {
      val listOfReactants = reaction.getListOfReactants.asScala.toList.map(makeReactantObject(_, false))
      val listOfProducts = reaction.getListOfProducts.asScala.toList.map(makeReactantObject(_, true))
      listOfReactants.foreach(_.upload(graphDataBaseConnection))
      listOfProducts.foreach(_.upload(graphDataBaseConnection))
//      val listOfReactants = reactantObjects.map(_.upload(graphDataBaseConnection))
//      val listOfProducts = productObjects.map(_.upload(graphDataBaseConnection))
      val properties = Map("reversible" -> reaction.isReversible, "metaId" -> reaction.getMetaId)
      Reaction(name = reaction.getName, reactants = listOfReactants, products = listOfProducts, properties = properties)
    }


//    val geneProducts = makeGeneProductObject()

    val reactionObjects = reactions.map(makeReactionObject)
    reactionObjects.map(_.upload(graphDataBaseConnection))
  }

//  def makeGeneProductObject(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct): Polypeptide = {
//    val geneName = geneProduct.getName
//    val metaId = geneProduct.getMetaId
//    val locus_tag = geneProduct.getLabel
//    if (geneProductCollector.contains(metaId)) geneProductCollector(metaId)
//    else {
////      Polypeptide(geneName, Gene)
//      geneProductCollector(metaId)
//    }
//  }

  def getPolypeptideByLocusTag(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct): Map[String, org.neo4j.graphdb.Node] = {
    val geneNode = Option(
      graphDataBaseConnection.findNode(
        DynamicLabel.label("Gene"), "locus_tag", geneProduct.getLabel
      )
    )
    val result = geneNode match {
      case Some(gn) =>
        val polypeptideNode = gn.getSingleRelationship(BiomeDBRelations.encodes, Direction.OUTGOING).getEndNode
//        val props = polypeptideNode.getPropertyKeys.asScala.toMap
        Some(geneProduct.getMetaId, polypeptideNode)
      case None => None
    }
    result.flatten.toMap
  }

}

