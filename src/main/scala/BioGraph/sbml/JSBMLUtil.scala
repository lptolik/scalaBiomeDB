package BioGraph.sbml

import java.io.File

import BioGraph.{Compartment, Reaction, _}
import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{Direction, DynamicLabel}
import org.sbml.jsbml._
import org.sbml.jsbml.ext.SBasePlugin
import org.sbml.jsbml.ext.fbc.{GeneProduct, _}
import utilFunctions.{BiomeDBRelations, TransactionSupport}
import utilFunctions.utilFunctionsObject._

import scala.collection.JavaConverters._
import scala.collection.immutable.Map
import scala.util.Try

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
  val reactome = reactomeInfo._1
//  create dictionaries of Compounds and their Chebi and Reactome XRefs
  val totalChebiCompoundCollector: Map[String, Compound] =
    getNodesDict(graphDataBaseConnection)(getCompoundPropertiesByXRefs, "XRef")(_.getProperty("id").toString.contains("CHEBI:"))
      .filter(elem => elem._1.contains("CHEBI"))
  val totalReactomeCompoundCollector: Map[String, Compound] =
    getNodesDict(graphDataBaseConnection)(getCompoundPropertiesByXRefs, "XRef")(_.getProperty("id").toString.contains("reactome"))
      .filter(elem => elem._1.contains("reactome"))
  var totalCompoundCollector = totalChebiCompoundCollector ++ totalReactomeCompoundCollector
//  collectors of Reactants and GeneProducts
  var reactantCollector: Map[String, Reactant] = Map()
  var geneProductCollector: Map[String, org.neo4j.graphdb.Node] = Map()
  var enzymeCollector: Map[Set[org.neo4j.graphdb.Node], org.neo4j.graphdb.Node] = Map()
  var mapOfModelParameters: Map[String, Double] = Map()

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

  private def checkModelOrganismName(organismName: String): Boolean = {
    val searching = Option(graphDataBaseConnection.findNode(DynamicLabel.label("Organism"), "name", organismName))
    val res =  searching match {
      case Some(o) => true
      case None => false
    }
    res
  }

  def getPolypeptideByLocusTag(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct): Option[(String, org.neo4j.graphdb.Node)] = {
    val geneNode = Option(
      graphDataBaseConnection.findNode(
        DynamicLabel.label("Gene"), "locus_tag", geneProduct.getLabel
      )
    ).orElse {Option(
      graphDataBaseConnection.findNode(
        DynamicLabel.label("Gene"), "name", geneProduct.getLabel
      )
    )}
    val sbmlIdPolyPair = geneNode match {
      case Some(gn) =>
        val polypeptideNode = gn.getSingleRelationship(BiomeDBRelations.encodes, Direction.OUTGOING).getEndNode
        val props = polypeptideNode.getProperties().asScala.toMap
        Some(geneProduct.getId, polypeptideNode)
      case None => None
    }
    sbmlIdPolyPair
  }

  def getFBCReaction(fbcReaction: SBasePlugin): Option[FBCReactionPlugin] = {
    val reaction = fbcReaction match {
      case r:FBCReactionPlugin => Some(r)
      case _ => None
    }
    reaction
  }

  @throws[IllegalArgumentException]
  def uploadModel(parsedModel: Model) = transaction(graphDataBaseConnection) {

    checkModelOrganismName(parsedModel.getName) match {
      case false => throw new IllegalArgumentException("There is no organism with the name " + parsedModel.getName)
      case true =>
    }

    // try to get fbc information from the SBML model
    val fbcModel = parsedModel.getModel.getPlugin("fbc")
    mapOfModelParameters ++= parsedModel.getListOfParameters.asScala.map(p => (p.getId, p.getValue)).toMap

//    to do
//    put this data from annotation to graph
//    val tax = parsedModel.getAnnotation.getFullAnnotation.getChild(1).getChild(0).getChild(5).getChild(1).getChild(1).getAttrValue(0)

    fbcModel match {
      case model: FBCModelPlugin =>
        val listOfGeneProducts = model.getListOfGeneProducts.asScala.toList
        geneProductCollector ++= listOfGeneProducts
          .map { gp =>
            getPolypeptideByLocusTag(gp)
              .getOrElse(createPolypeptide(gp))
          }.toMap

        logger.info(s"Gene product count: ${geneProductCollector.size}")

        enzymeCollector ++= getEnzymes
      case _ =>
    }

    def createPolypeptide(gp: GeneProduct) = {
      val createdGeneProduct = graphDataBaseConnection.createNode(DynamicLabel.label("Polypeptide"))
      val sbmlId = gp.getId
      createdGeneProduct.addLabel(DynamicLabel.label("To_check"))
      createdGeneProduct.setProperty("sbmlId", sbmlId)
      createdGeneProduct.setProperty("label", gp.getLabel)
      createdGeneProduct.setProperty("metaId", gp.getMetaId)
      (sbmlId, createdGeneProduct)
    }

    val fbcReactions = parsedModel.getListOfReactions.asScala.toList.map(_.getPlugin("fbc")).flatMap(getFBCReaction)
    val reactions = parsedModel.getListOfReactions.asScala.toList
    val zipFBCReactions = reactions.zip(fbcReactions)
    val currentReaction = ReactionReader(parsedModel)(_)

    def processOneReaction(zipFBCReaction: (org.sbml.jsbml.Reaction, FBCReactionPlugin)) = {
      val r = currentReaction(zipFBCReaction)
      val reactionObject = r.makeReactionObject
      val associationsNodes = r.getGeneProductsAssociations
      val reactionNode = reactionObject.upload(graphDataBaseConnection)
      associationsNodes.foreach(_.createRelationshipTo(reactionNode, BiomeDBRelations.catalyzes))
      reactionNode
    }
    val uploadedReactions = zipFBCReactions.map(processOneReaction)
    uploadedReactions
  }



  def getEnzymes: Map[Set[org.neo4j.graphdb.Node], org.neo4j.graphdb.Node] = {
    graphDataBaseConnection
      .findNodes(DynamicLabel.label("Enzyme"))
      .asScala
      .map {
        enzymeNode =>
          val enzymePolys = enzymeNode.getRelationships(Direction.INCOMING, BiomeDBRelations.partOf).asScala
            .map(_.getEndNode).toSet
          (enzymePolys, enzymeNode)
      }.toMap
  }

  case class ReactionReader(parsedModel: Model)(zipFBCReaction: (org.sbml.jsbml.Reaction, FBCReactionPlugin)) {
    val reaction = zipFBCReaction._1
    val fbcReaction = zipFBCReaction._2
    val reactionName = reaction.getName
    def getPolypeptideByLocusTag(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct): Option[(String, org.neo4j.graphdb.Node)] = {
      val geneNode = Option(
        graphDataBaseConnection.findNode(
          DynamicLabel.label("Gene"), "locus_tag", geneProduct.getLabel
        )
      )
      val sbmlIdPolyPair = geneNode match {
        case Some(gn) =>
          val polypeptideNode = gn.getSingleRelationship(BiomeDBRelations.encodes, Direction.OUTGOING).getEndNode
          Some(geneProduct.getId, polypeptideNode)
        case None => None
      }
      sbmlIdPolyPair
    }

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
      val sbmlId = specie.getId
      val specieFBC = specie.getPlugin("fbc")
      val stoiFactor = productFlag match {
        case true => 1
        case false => -1
      }

      val compounds = makeCompoundObject(specie, specieName)
      val toCheck = compounds.isEmpty match {
        case true => true
        case false => false
      }

      reactantCollector.getOrElse(sbmlId, {
        //      make return as reactant and several compounds
        val formula = specieFBC match {
          case fbc: FBCSpeciesPlugin => Some(fbc.getChemicalFormula)
          case _ => None
        }
        val charge = specieFBC match {
          case fbc: FBCSpeciesPlugin => Some(fbc.getCharge)
          case _ => None
        }
        val r = Reactant(
          name = specieName,
          stoichiometry = Some(stoiFactor * stoi),
          compartment = Some(compartmentNodes(compartment)),
          compounds = compounds,
          formula = formula,
          charge = charge,
          toCheck = toCheck,
          properties = Map(
            "sbmlId" -> sbmlId,
            "metaId" -> metaId,
            "sboTerm" -> specie.getSBOTerm
          )
        )
        reactantCollector ++= Map(sbmlId -> r)
        r
      })
    }

    def makeReactionObject: Reaction = {
      val listOfReactants = reaction.getListOfReactants.asScala.toList.map(makeReactantObject(_, false))
      val listOfProducts = reaction.getListOfProducts.asScala.toList.map(makeReactantObject(_, true))
      listOfReactants.foreach(_.upload(graphDataBaseConnection))
      listOfProducts.foreach(_.upload(graphDataBaseConnection))
      //      enzyme needs name of reaction and its node or object
      //      so it can have relationship with it
      //      the result must be a List
      //      so it can be flattened
      //      make a special class for reaction to read it
      val lowerFluxMap = Option(mapOfModelParameters(fbcReaction.getLowerFluxBound)) match {
        case Some(v) => Map("lowerFluxBound" -> v)
        case None => Map()
      }

      val upperFluxMap = Option(mapOfModelParameters(fbcReaction.getUpperFluxBound)) match {
        case Some(v) => Map("upperFluxBound" -> v)
        case None => Map()
      }

      val properties = Map(
        "reversible" -> reaction.isReversible,
        "metaId" -> reaction.getMetaId,
        "sbmlId" -> reaction.getId) ++
        upperFluxMap ++
        lowerFluxMap
      Reaction(
        name = reactionName,
        reactants = listOfReactants,
        products = listOfProducts,
        properties = properties)
    }

    def getGeneProductsAssociations: List[org.neo4j.graphdb.Node] = {
//      fbcReaction.getLowerFluxBound
      val geneAssociations = reactionLoop(fbcReaction)
      geneAssociations
    }

    def processAndOperator(listOfAssociations: List[Association]): List[org.neo4j.graphdb.Node] = {
      val res = listOfAssociations match {
        case lgp: List[GeneProductRef] =>
          val polys = lgp.map(geneProduct).toSet
          if (enzymeCollector.contains(polys)) List(enzymeCollector(polys))
          else {
            val enzyme = Enzyme(name = reactionName)
            val enzymeNode = enzyme.upload(graphDataBaseConnection)
            polys.foreach(_.createRelationshipTo(enzymeNode, BiomeDBRelations.partOf))
            enzymeCollector ++= Map(polys -> enzymeNode)
            List(enzymeNode)
          }
        case _ =>
          logger.error("Not a GeneProductRef: " + listOfAssociations)
          List()
      }
      res
    }

    def geneProduct(gpa: GeneProductRef): org.neo4j.graphdb.Node = geneProductCollector(gpa.getGeneProduct)

    def reactionLoop(fbcReaction: FBCReactionPlugin): List[org.neo4j.graphdb.Node] = {
      Try(fbcReaction.getGeneProductAssociation.getAssociation)
        .toOption
        .map(a => processGeneProductAssociation(a))
        .getOrElse(List.empty)
    }

    def processGeneProductAssociation(association: Association, acc: List[org.neo4j.graphdb.Node] = List.empty)
    : List[org.neo4j.graphdb.Node] = {
      association match {
        case ref: GeneProductRef => acc :+ geneProduct(ref)
        case or: Or =>
          acc ++ or.getListOfAssociations.asScala.flatMap(ass => processGeneProductAssociation(ass, acc))
        case and: And =>
          processAndOperator(and.getListOfAssociations.asScala.toList)
      }
    }
  }

}
