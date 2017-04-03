package BioGraph.sbml

import java.io.File

import BioGraph.{BiochemicalReaction, Compartment, _}
import org.apache.log4j.{Level, Logger}
import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{Direction, DynamicLabel, Node}
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
//  var compartmentNodes = Map[String, Compartment]()
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
  val totalSynonymsCompoundCollector = transaction(graphDataBaseConnection) {
    graphDataBaseConnection
      .findNodes(DynamicLabel.label("Term"))
      .asScala
      .flatMap { termNode =>
        termNode
          .getRelationships(BiomeDBRelations.hasName, Direction.INCOMING)
          .asScala
          .map(_.getStartNode)
          .find(_.hasLabel(DynamicLabel.label("Compound")))
          .map { compoundNode =>
            val text = termNode.getProperty("text").toString
            val compound = Compound(compoundNode.getProperty("name").toString, nodeId = compoundNode.getId)
            text -> compound
          }
      }.toMap
  }

  var totalCompoundCollector = totalChebiCompoundCollector ++
    totalReactomeCompoundCollector ++
    totalSynonymsCompoundCollector

  //  collectors of Reactants and GeneProducts
  var reactantCollector: Map[String, Reactant] = Map()
  var geneProductCollector: Map[String, org.neo4j.graphdb.Node] = Map()
  var enzymeCollector: Map[Set[org.neo4j.graphdb.Node], org.neo4j.graphdb.Node] = Map()

  def getDataBasesNodes(dbName: String): (DBNode, Long) = transaction(graphDataBaseConnection){
    val db = DBNode(dbName)
    val dbNodeId = db.upload(graphDataBaseConnection).getId
    (db, dbNodeId)
  }

  def jsbmlModelFromFile(fileName: File): Model = transaction(graphDataBaseConnection){
//    get one model
    val readResult = reader.readSBML(fileName)
    val parsedModel = readResult.getModel
    parsedModel
  }

  def getPolypeptideByLocusTagOrGeneName(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct, organism: Organism)
  : Option[(String, org.neo4j.graphdb.Node)] = {

    val cypher =
      s"MATCH (o:Organism {name: '${organism.name}'})<-[:PART_OF]-(g:Gene)-[:ENCODES]->(p:Polypeptide) " +
        s"WHERE g.locus_tag = '${geneProduct.getLabel}' OR g.name = '${geneProduct.getLabel}' " +
        s"RETURN p"

    val resultIter = graphDataBaseConnection.execute(cypher).columnAs[Node]("p")
    if (resultIter.hasNext)
      Some((geneProduct.getId, resultIter.next()))
    else
      None
  }

  def findOrganism(name: String): Organism = {
    def fromNode(node: Node): Organism = {
      val props = node.getAllProperties
      //FIXME what to do with other properties? Maybe nothing?
      Organism(props.get("name").toString, List(props.get("source").toString), nodeId = node.getId)
    }

    Option(graphDataBaseConnection.findNode(DynamicLabel.label("Organism"), "name", name)) match {
      case Some(o) => fromNode(o)
      case None => throw new IllegalArgumentException(s"Organism with name '$name' not found")
    }
  }

  def uploadModel(organismName: String, spontaneousReactionsIds: Set[String])
                 (model: Model): List[Node] = transaction(graphDataBaseConnection) {
    val organism = findOrganism(organismName)
    // try to get fbc information from the SBML model
    val fbcModel = model.getModel.getPlugin("fbc").asInstanceOf[FBCModelPlugin]

    val compartmentNodes = model
      .getListOfCompartments.asScala
      .toList
      .map(elem => elem.getId -> Compartment(elem.getName, organism))
      .toMap

    val modelParameters = model.getListOfParameters.asScala.map(p => (p.getId, p.getValue)).toMap

    val listOfGeneProducts = fbcModel.getListOfGeneProducts.asScala.toList
    geneProductCollector ++= listOfGeneProducts
      .map { gp =>
        getPolypeptideByLocusTagOrGeneName(gp, organism)
          .getOrElse(createPolypeptide(gp, organism))
      }.toMap

    logger.info(s"Gene product count: ${geneProductCollector.size}")

    enzymeCollector ++= getEnzymes

    def createPolypeptide(gp: GeneProduct, organism: Organism) = {
      val createdGeneProduct = graphDataBaseConnection.createNode(DynamicLabel.label("Polypeptide"))
      val sbmlId = gp.getId
      createdGeneProduct.addLabel(DynamicLabel.label("To_check"))
      createdGeneProduct.setProperty("sbmlId", sbmlId)
      createdGeneProduct.setProperty("label", gp.getLabel)
      createdGeneProduct.setProperty("metaId", gp.getMetaId)

      val organismNode = graphDataBaseConnection.getNodeById(organism.getId)
      createdGeneProduct.createRelationshipTo(organismNode, BiomeDBRelations.partOf)

      (sbmlId, createdGeneProduct)
    }

    val fbcReactions = model.getListOfReactions.asScala.toList.map(_.getPlugin("fbc").asInstanceOf[FBCReactionPlugin])
    val reactions = model.getListOfReactions.asScala.toList
    val zipFBCReactions = reactions.zip(fbcReactions)
    val currentReaction = ReactionReader(model)(_)

    def processOneReaction(zipFBCReaction: (org.sbml.jsbml.Reaction, FBCReactionPlugin)) = {
      val r = currentReaction(zipFBCReaction)
      val reactionObject = r.makeReactionObject(compartmentNodes, organism, modelParameters, spontaneousReactionsIds)
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
      .map { enzymeNode =>
        val enzymePolys = enzymeNode.getRelationships(Direction.INCOMING, BiomeDBRelations.partOf).asScala
          .map(_.getEndNode).toSet
        (enzymePolys, enzymeNode)
      }.toMap
  }

  case class ReactionReader(parsedModel: Model)(zipFBCReaction: (org.sbml.jsbml.Reaction, FBCReactionPlugin)) {
    val reaction = zipFBCReaction._1
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
        totalCompoundCollector.getOrElse(shortXRefName, {
          logger.warn("No compound was found by XRef id:" + xrefName)
          val createdCompound = Compound(specieName, toCheck = true)
          val createdXRef = XRef(shortXRefName, db)
          val createdCompoundNode = createdCompound.upload(graphDataBaseConnection)
          val createdXRefNode = createdXRef.upload(graphDataBaseConnection)
          createdCompoundNode.createRelationshipTo(createdXRefNode, BiomeDBRelations.evidence)
          totalCompoundCollector ++= Map(shortXRefName -> createdCompound)
          createdCompound
        })
      }

      val compoundsByXref = chebiXRefs.map(getOrCreateCompoundNode(_, chebi)) ++
        reactomeXRefs.map(getOrCreateCompoundNode(_, reactome))

      val compoundByNameOpt = totalCompoundCollector
        .get(specieName)
        .orElse {
          if (compoundsByXref.nonEmpty)
            None
          else {
            val createdCompound = Compound(specieName, toCheck = true)
            createdCompound.upload(graphDataBaseConnection)
            totalCompoundCollector ++= Map(specieName -> createdCompound)
            Some(createdCompound)
          }
        }

      compoundByNameOpt
        .map(compoundByName => compoundByName :: compoundsByXref)
        .getOrElse(compoundsByXref)
        .distinct
    }

    def makeReactantObject(speciesReference: SpeciesReference,
                           compartmentNodes: Map[String, Compartment],
                           isProduct:Boolean = false): Reactant = {
      val specie = parsedModel.getSpecies(speciesReference.getSpecies)
      val specieName = specie.getName
      val compartment = specie.getCompartment
      val stoi = if (isProduct) speciesReference.getStoichiometry else -speciesReference.getStoichiometry
      val metaId = specie.getMetaId
      val sbmlId = specie.getId
      val specieFBC = specie.getPlugin("fbc")

      val compounds = makeCompoundObject(specie, specieName)
      val toCheck = compounds.isEmpty

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
          stoichiometry = Some(stoi),
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
        reactantCollector += sbmlId -> r
        r
      }).copy(stoichiometry = Some(stoi))
    }

    def makeReactionObject(compartmentNodes: Map[String, Compartment],
                           organism: Organism,
                           parameters: Map[String, Double],
                           spontaneousReactionsIds: Set[String]): BiochemicalReaction = {

      val listOfReactants = reaction.getListOfReactants.asScala.toList.map(makeReactantObject(_, compartmentNodes, isProduct = false))
      val listOfProducts = reaction.getListOfProducts.asScala.toList.map(makeReactantObject(_, compartmentNodes, isProduct = true))
      listOfReactants.foreach(_.upload(graphDataBaseConnection))
      listOfProducts.foreach(_.upload(graphDataBaseConnection))

      val isSpontaneous = reactionHasSpontaneousGeneProductRef(spontaneousReactionsIds)

      val properties = Map(
        "reversible" -> reaction.isReversible,
        "metaId" -> reaction.getMetaId,
        "sbmlId" -> reaction.getId,
        "lowerFluxBound" -> parameters(zipFBCReaction._2.getLowerFluxBound),
        "upperFluxBound" -> parameters(zipFBCReaction._2.getUpperFluxBound)
      )
      BiochemicalReaction(
        name = reaction.getName,
        reactants = listOfReactants,
        products = listOfProducts,
        organism = Some(organism),
        properties = properties,
        isSpontaneous = isSpontaneous
      )
    }

    def reactionHasSpontaneousGeneProductRef(spontaneousReactionsIds: Set[String]): Boolean = {
      Try(zipFBCReaction._2.getGeneProductAssociation.getAssociation)
        .toOption.exists {
          case gpr: GeneProductRef => spontaneousReactionsIds.contains(gpr.getGeneProduct)
          case or: Or => or
            .getListOfAssociations.asScala
            .flatMap(a => Try(a.asInstanceOf[GeneProductRef]).toOption)
            .exists(gpr => spontaneousReactionsIds.contains(gpr.getGeneProduct))
          case _ => false
      }
    }

    def getGeneProductsAssociations: List[org.neo4j.graphdb.Node] = {
      val fbcReaction = zipFBCReaction._2
      val geneAssociations = reactionLoop(fbcReaction)
      geneAssociations
    }

    def getComplexAlternatives(associations: List[Association]): List[List[GeneProductRef]] = {
      associations.foldLeft(List(List.empty[GeneProductRef])) {
        case (res, next: GeneProductRef) =>
          res.map(prev => next :: prev)
        case (res, next: Or) =>
          next.getListOfAssociations.asScala.toList.flatMap { case gpr: GeneProductRef =>
              res.map(prev => gpr :: prev)
          }
      }
    }

    def processAndOperator(listOfAssociations: List[Association]): List[org.neo4j.graphdb.Node] = {
      val alternatives = getComplexAlternatives(listOfAssociations)

      alternatives.flatMap {
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
          throw new Exception("Not a GeneProductRef: " + listOfAssociations)
      }
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
