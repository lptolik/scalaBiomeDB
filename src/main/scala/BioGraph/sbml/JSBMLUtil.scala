package BioGraph.sbml

import java.io.File

import BioGraph.{BiochemicalReaction, Compartment, _}
import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{Direction, DynamicLabel, GraphDatabaseService, Node}
import org.sbml.jsbml._
import org.sbml.jsbml.ext.fbc.{GeneProduct, _}
import utilFunctions.{BiomeDBRelations, TransactionSupport}
import utilFunctions.utilFunctionsObject._

import scala.collection.JavaConverters._
import scala.collection.immutable.Map
import scala.util.Try

/**
  * Created by artem on 14.07.16.
  */
class JSBMLUtil(graphDataBaseConnection: GraphDatabaseService) extends TransactionSupport {
  val logger = LogManager.getLogger(this.getClass.getName)
//  val graphDataBaseConnection: GraphDatabaseService = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  val reader = new SBMLReader()
//  get dictionary of ChEBI and Reactome XRefs
  val chebiInfo = getDataBasesNodes("ChEBI")
  val reactomeInfo = getDataBasesNodes("Reactome")
  val chebi = chebiInfo._1
  val reactome = reactomeInfo._1
  val totalReactomeAndChebiCompoundCollector: Map[String, Compound] =
    getNodesDict(graphDataBaseConnection)(getCompoundPropertiesByXRefs, "XRef"){
      x =>
        val xrefID = x.getProperty("id").toString.toLowerCase
        xrefID.contains("reactome") || xrefID.contains("chebi")
    }

  val totalCompoundNameCollector: Map[String, Compound] =
    utilFunctions.utilFunctionsObject.makeNodesDict("Compound", "name", n => n.toLowerCase.trim)(graphDataBaseConnection)

  val totalSynonymsCompoundCollector = transaction(graphDataBaseConnection) {
    graphDataBaseConnection.findNodes(DynamicLabel.label("Compound"))
      .asScala
      .flatMap{cn =>
        val compoundObject = Compound(cn)
        cn.getRelationships(BiomeDBRelations.hasName, Direction.OUTGOING)
        .asScala
        .map(_.getEndNode)
        .map(t =>
          t.getProperty("text").toString -> compoundObject)//Compound(compoundName, nodeId = compoundId))
      }.toMap
  }

  var totalCompoundCollector =
    (totalReactomeAndChebiCompoundCollector ++
    totalSynonymsCompoundCollector ++
    totalCompoundNameCollector).par

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

  def getPolypeptideByLocusTagOrGeneName(locusTagOrGeneName: String, organismName: String, id: String)
  : Option[(String, org.neo4j.graphdb.Node)] = {

    val cypher =
      s"MATCH (o:Organism {name: '$organismName'})<-[:PART_OF]-(g:Gene)-[:ENCODES]->(p:Polypeptide) " +
        s"WHERE g.locus_tag = '$locusTagOrGeneName' OR g.name = '$locusTagOrGeneName' " +
        s"RETURN p"

    val resultIter = graphDataBaseConnection.execute(cypher).columnAs[Node]("p")
    if (resultIter.hasNext)
      Some((id, resultIter.next()))
    else
      None
  }

  def getPolypeptideByFigPegXRef(figPegId: String, organismName: String, taxonId: Int, id: String)
  : Option[(String, org.neo4j.graphdb.Node)] = {

    if (figPegId.startsWith("g.")) //e.g. "g.2189.peg.1511"
      None
    else {
      val realLabel = figPegId.split('.').drop(2).mkString("\\\\.") //handle case of 0000000.0 taxon id in AGORA models
      val figXrefRegexp = s"fig\\\\|$taxonId\\\\.[0-9]\\\\.$realLabel"

      val cypher =
        s"MATCH (o:Organism {name: '$organismName'})<-[:PART_OF]-(p:Polypeptide)" +
          s"-[:EVIDENCE]->(x:XRef)-[:LINK_TO]->(:DB {name: 'SEED'}) " +
          s"WHERE x.id =~ '$figXrefRegexp' " +
          s"RETURN p"

      val resultIter = graphDataBaseConnection.execute(cypher).columnAs[Node]("p")
      if (resultIter.hasNext)
        Some((id, resultIter.next()))
      else
        None
    }
  }

  def getPolypeptideBySequenceIdentity(locusTagOrGeneName: String, organismName: String, id: String)
  : Option[(String, org.neo4j.graphdb.Node)] = {

    val cypher =
      s"MATCH (g:Gene)-[:ENCODES]->(:Polypeptide)-[:IS_A]->(:AA_Sequence)<-[:IS_A]-(p:Polypeptide)" +
        s"-[:PART_OF]->(o:Organism {name: '$organismName'}) " +
        s"WHERE g.locus_tag = '$locusTagOrGeneName' OR g.name = '$locusTagOrGeneName' " +
        s"RETURN p"

    val resultIter = graphDataBaseConnection.execute(cypher).columnAs[Node]("p")
    if (resultIter.hasNext)
      Some((id, resultIter.next()))
    else
      None
  }

  private def createOrganismObject(node: Node): Organism = {
    val props = node.getAllProperties
    val res = Organism(props.get("name").toString, List(props.get("source").toString), nodeId = node.getId)
    res
  }

  def findOrganismByName(name: String): Option[Organism] = {
    val warnMessage = s"Organism with name '$name' not found"
    val res  = Try(graphDataBaseConnection.findNode(DynamicLabel.label("Organism"), "name", name)).toOption match {
      case Some(o: Node) => Option(createOrganismObject(o))
      case Some(null) =>
        logger.warn(warnMessage)
        None
      case None =>
        logger.warn(warnMessage)
        None
    }
    res
  }

  def findOrganismByTaxon(taxonID: Int): Option[Organism] = {
    val warnMessage = s"Taxon with tax_id '$taxonID' not found"
    Option(graphDataBaseConnection.findNode(DynamicLabel.label("Taxon"), "tax_id", taxonID)) match {
      case Some(t: Node) =>
        Try(createOrganismObject(t
          .getSingleRelationship(BiomeDBRelations.isA, Direction.INCOMING)
          .getStartNode)).toOption
      case Some(null) =>
        logger.warn(warnMessage)
        None
      case None =>
        logger.warn(warnMessage)
        None
    }
  }

  def getTaxonFromModel(model: Model): Int = {
    val taxonID = model
      .getAnnotation
      .getListOfCVTerms
      .asScala
      .map(_.getResources)
      .filter(_.toString.contains("taxonomy"))
      .toString
      .split("taxonomy/")(1)
      .dropRight(2)
      .toInt
    taxonID
  }

  def uploader(sourceDB: String, model: Model, spontaneousReactionsIds: Set[String]): scala.Unit =
    transaction(graphDataBaseConnection) {
      val organismName = model.getName
      val organismByName = findOrganismByName(organismName)
      val taxonId = getTaxonFromModel(model)
      val organismByTaxon = findOrganismByTaxon(taxonId)

      val organism = organismByTaxon match {
        case Some(byTaxon) =>
          logger.info("Model matched by taxon.")
          byTaxon
        case None => organismByName match {
          case Some(byName) =>
            logger.info("Model matched by organism name.")
            byName
          case None =>
            println(s"Organism for model ${model.getName} not found")
            logger.warn(s"Organism for model ${model.getName} not found")
            None
        }
      }

      organism match {
        case o: Organism => uploadModel(o, taxonId, sourceDB, spontaneousReactionsIds)(model)
        case None =>
      }

    }

  private def uploadModel(organism: Organism, taxonId: Int, sourceDB: String, spontaneousReactionsIds: Set[String])
                         (model: Model): scala.Unit = transaction(graphDataBaseConnection) {

    val modelNode = ModelNode(model.getId, sourceDB).upload(graphDataBaseConnection)
    modelNode.setProperty("name", organism.name)
    val organismNode = graphDataBaseConnection.getNodeById(organism.getId)

    modelNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)

    // try to get fbc information from the SBML model
    val fbcModel = model.getModel.getPlugin("fbc").asInstanceOf[FBCModelPlugin]

    val compartmentNodes = model
      .getListOfCompartments
      .asScala
      .map(elem => elem.getId -> Compartment(elem.getName, organism))
      .toMap

    val modelParameters = model.getListOfParameters.asScala.map(p => (p.getId, p.getValue)).toMap

    val listOfGeneProducts = fbcModel.getListOfGeneProducts.asScala.toList
    geneProductCollector ++= listOfGeneProducts
      .map { gp =>
        getPolypeptideByLocusTagOrGeneName(gp.getLabel, organism.name, gp.getId)
          .orElse(getPolypeptideByLocusTagOrGeneName(gp.getName, organism.name, gp.getId))
          .orElse(getPolypeptideBySequenceIdentity(gp.getLabel, organism.name, gp.getId))
          .orElse(getPolypeptideBySequenceIdentity(gp.getName, organism.name, gp.getId))
          .orElse(getPolypeptideByFigPegXRef(gp.getLabel, organism.name, taxonId, gp.getId))
          .getOrElse(createPolypeptide(gp, organism))
      }.toMap

    logger.info(s"Gene product count: ${geneProductCollector.size}")

    enzymeCollector ++= getEnzymes

    def createPolypeptide(gp: GeneProduct, organism: Organism) = {
      val createdGeneProduct = graphDataBaseConnection.createNode(DynamicLabel.label("Polypeptide"))
      val sbmlId = gp.getId
      createdGeneProduct.addLabel(DynamicLabel.label("To_check"))
      createdGeneProduct.setProperty("sbmlId", sbmlId)
      createdGeneProduct.setProperty("name", gp.getLabel + "_" + model.getId)
      createdGeneProduct.setProperty("label", gp.getLabel)
      createdGeneProduct.setProperty("metaId", gp.getMetaId)

      createdGeneProduct.createRelationshipTo(organismNode, BiomeDBRelations.partOf)

      (sbmlId, createdGeneProduct)
    }

    val fbcReactions = model
      .getListOfReactions
      .asScala
      //.toList
      .map(_.getPlugin("fbc")
      .asInstanceOf[FBCReactionPlugin])
    val reactions = model.getListOfReactions.asScala//.toList
    val zipFBCReactions = reactions.zip(fbcReactions)
    val getCurrentReaction = ReactionReader(model)(_)

    def processOneReaction(zipFBCReaction: (org.sbml.jsbml.Reaction, FBCReactionPlugin)): scala.Unit = {
      val r = getCurrentReaction(zipFBCReaction)
      val reactionObject = r.makeReactionObject(compartmentNodes, organism, modelParameters, spontaneousReactionsIds)
      val associationsNodes = r.getGeneProductsAssociations
      val reactionNode = reactionObject.upload(graphDataBaseConnection)
      reactionNode.createRelationshipTo(modelNode, BiomeDBRelations.partOf)
      associationsNodes.foreach(_.createRelationshipTo(reactionNode, BiomeDBRelations.catalyzes))
    }
    zipFBCReactions.foreach(processOneReaction)
  }

  def getEnzymes: Map[Set[org.neo4j.graphdb.Node], org.neo4j.graphdb.Node] = {
    graphDataBaseConnection
      .findNodes(DynamicLabel.label("Enzyme"))
      .asScala
      .map { enzymeNode =>
        val enzymePolys = enzymeNode
          .getRelationships(Direction.INCOMING, BiomeDBRelations.partOf)
          .asScala
          .map(_.getEndNode)
          .toSet
        (enzymePolys, enzymeNode)
      }
      .toMap
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

    def makeCompoundObject(specie: Species, speciesName: String): List[Compound] = transaction(graphDataBaseConnection) {
      val chebiXRefs = Try(
        specie
          .getCVTerms
          .asScala
          .head
          .getResources
          .asScala
          .toList
          .filter(ref => ref.toLowerCase().contains("chebi"))
      ).toOption match {
        case Some(o:List[String]) => o
        case _ => List()
      }
      val reactomeXRefs = Try(
        specie
          .getCVTerms
          .asScala
          .head
          .getResources
          .asScala
          .toList
          .filter(_.toLowerCase.contains("reactome"))
      ).toOption match {
        case Some(o: List[String]) => o
        case _ => List()
      }

      //Logic of the code below is the following:
      //1. Find compound by XRefs without creation (for now) of new nodes
      //2. If compounds were found by XRefs, all ok
      //3. If no compounds were found by XRefs, then find it by name
      //4. If no compounds were found by name and there are links to chebi or reactome, then create compound by XRefs
      //5. If there are no links to chebi or reactome, create it by name

      def createToCheckCompound(xrefName: String, db: DBNode) = {
        val shortXRefName = xrefName.split("/").last
        logger.warn("No compound was found by XRef id:" + xrefName)
        val createdCompound = Compound(speciesName, toCheck = true)
        val createdXRef = XRef(shortXRefName, db)
        val createdCompoundNode = createdCompound.upload(graphDataBaseConnection)
        val createdXRefNode = createdXRef.upload(graphDataBaseConnection)
        createdCompoundNode.createRelationshipTo(createdXRefNode, BiomeDBRelations.evidence)
        totalCompoundCollector ++= Map(shortXRefName -> createdCompound)
        createdCompound
      }

      val foundByXRefs = chebiXRefs.flatMap(xref => totalCompoundCollector.get(xref.split("/").last)) ++
        reactomeXRefs.flatMap(xref => totalCompoundCollector.get(xref.split("/").last)).distinct

      val finalRes = if (foundByXRefs.nonEmpty)
        foundByXRefs
      else {
        totalCompoundCollector
          .get(speciesName.toLowerCase.trim)
          .map(List(_))
          .getOrElse {
            val toCheckByXRefCompounds = chebiXRefs.map(createToCheckCompound(_, chebi)) ++
              reactomeXRefs.map(createToCheckCompound(_, reactome))

            if (toCheckByXRefCompounds.nonEmpty)
              toCheckByXRefCompounds
            else {
              logger.warn("No compound was found by name: " + speciesName)
              val toCheckByNameCompound = Compound(speciesName, toCheck = true)
              toCheckByNameCompound.upload(graphDataBaseConnection)
              totalCompoundCollector ++= Map(speciesName.toLowerCase.trim -> toCheckByNameCompound)
              List(toCheckByNameCompound)
            }
          }
      }

      require(finalRes.nonEmpty, "No compounds were found or created (which means that the code contains stupid bugs)")

      finalRes
    }

    def makeReactantObject(speciesReference: SpeciesReference,
                           compartmentNodes: Map[String, Compartment],
                           isProduct:Boolean = false): Reactant = {
      val species = parsedModel.getSpecies(speciesReference.getSpecies)
      val sbmlId = species.getId
      val stoi = if (isProduct) speciesReference.getStoichiometry else -speciesReference.getStoichiometry

      reactantCollector.getOrElse(sbmlId, {

        val speciesName = species.getName
        val compounds = makeCompoundObject(species, speciesName)
        val toCheck = compounds.isEmpty
        val speciesFBC = species.getPlugin("fbc").asInstanceOf[FBCSpeciesPlugin]
        val compartment = species.getCompartment
        val metaId = species.getMetaId

        //      make return as reactant and several compounds
        val formula = Try(speciesFBC.getChemicalFormula).toOption
        val charge = Try(speciesFBC.getCharge).toOption
        val r = Reactant(
          name = speciesName,
          stoichiometry = Some(stoi),
          compartment = Some(compartmentNodes(compartment)),
          compounds = compounds,
          formula = formula,
          charge = charge,
          toCheck = toCheck,
          properties = Map(
            "sbmlId" -> sbmlId,
            "metaId" -> metaId,
            "sboTerm" -> species.getSBOTerm
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

      val isSpontaneous = reactionName.toLowerCase.contains("spontaneous") ||
        reactionHasSpontaneousGeneProductRef(spontaneousReactionsIds)
      val ecNumberStrings = reaction.getCVTerms
        .asScala
        .flatMap { xRefs =>
          xRefs
            .getResources
            .asScala
            .filter(_.toLowerCase().contains("ec-code"))
            .flatMap(_.split("/").last.split(",").map(_.trim))
        }

      val properties = Map(
        "reversible" -> reaction.isReversible,
        "metaId" -> reaction.getMetaId,
        "sbmlId" -> reaction.getId,
        "lowerFluxBound" -> parameters(zipFBCReaction._2.getLowerFluxBound),
        "upperFluxBound" -> parameters(zipFBCReaction._2.getUpperFluxBound)
      )
      BiochemicalReaction(
        name = reactionName,
        reactants = listOfReactants,
        products = listOfProducts,
        organism = Some(organism),
        properties = properties,
        isSpontaneous = isSpontaneous,
        ecNumberStrings = ecNumberStrings
      )
    }

    def reactionHasSpontaneousGeneProductRef(spontaneousReactionsIds: Set[String]): Boolean = {
      Try(zipFBCReaction._2.getGeneProductAssociation.getAssociation)
        .toOption.exists {
          case gpr: GeneProductRef => spontaneousReactionsIds
            .contains(gpr.getGeneProduct)
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
          next.getListOfAssociations.asScala.toList.flatMap {
            case gpr: GeneProductRef => res.map(prev => gpr :: prev)
            case and: And =>
              val andRefs = and.getListOfAssociations.asScala.map { case gpr: GeneProductRef => gpr }.toList
              res.map(prev => andRefs ++ prev)
          }
      }
    }

    def processAndOperator(listOfAssociations: List[Association]): List[org.neo4j.graphdb.Node] = {
      val alternatives = getComplexAlternatives(listOfAssociations)

      alternatives.flatMap {
        case lgp: List[GeneProductRef] =>
          val polys = lgp.map(geneProduct).toSet
          if (enzymeCollector.contains(polys))
            List(enzymeCollector(polys))
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
        case and: And => processAndOperator(and.getListOfAssociations.asScala.toList)
      }
    }
  }

}
