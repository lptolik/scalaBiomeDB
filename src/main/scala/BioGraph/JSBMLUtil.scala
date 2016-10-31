package BioGraph

import java.io.File

import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml._
import org.sbml.jsbml.ext.fbc._
import utilFunctions.TransactionSupport
import org.neo4j.graphdb.{Direction, DynamicLabel}
import org.sbml.jsbml.ext.SBasePlugin
import utilFunctions.utilFunctionsObject._
import utilFunctions.BiomeDBRelations

import scala.collection.JavaConverters._
import scala.collection.immutable.Map
//import scala.util.{Failure, Success, Try}

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

  def getPolypeptideByLocusTag(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct): Option[(String, org.neo4j.graphdb.Node)] = {
    val geneNode = Option(
      graphDataBaseConnection.findNode(
        DynamicLabel.label("Gene"), "locus_tag", geneProduct.getLabel
      )
    )
    val metaIdPolyPair = geneNode match {
      case Some(gn) =>
        val polypeptideNode = gn.getSingleRelationship(BiomeDBRelations.encodes, Direction.OUTGOING).getEndNode
        val props = polypeptideNode.getProperties().asScala.toMap
        //        val props = polypeptideNode.getPropertyKeys.asScala.toMap
        Some(geneProduct.getMetaId, polypeptideNode)
      case None => None
    }
    metaIdPolyPair
  }

  def getFBCReaction(fbcReaction: SBasePlugin): Option[FBCReactionPlugin] = {
    val reaction = fbcReaction match {
      case r:FBCReactionPlugin => Some(r)
      case _ => None
    }
    reaction
  }

  def uploadModels(parsedModel: Model) = transaction(graphDataBaseConnection) {
    // try to get fbc information from the SBML model
    val fbcModel = parsedModel.getModel.getPlugin("fbc")//.asInstanceOf[FBCModelPlugin]
    fbcModel match {
      case model: FBCModelPlugin =>
        val listOfGeneProducts = model.getListOfGeneProducts.asScala.toList
        geneProductCollector ++= listOfGeneProducts.flatMap(getPolypeptideByLocusTag).toMap
        enzymeCollector ++= getEnzymes
      case _ =>
    }

    val fbcReactions = parsedModel.getListOfReactions.asScala.toList.map(_.getPlugin("fbc")).flatMap(getFBCReaction)

    //      val fbcModel = parsedModel.getModel.getPlugin("fbc")//.asInstanceOf[FBCModelPlugin]
    //      fbcModel match {
    //        case model: FBCModelPlugin =>
    //          val listOfGeneProducts = model.getListOfGeneProducts.asScala.toList
    //          geneProductCollector ++= listOfGeneProducts.map(getPolypeptideByLocusTag).flatten.toMap
    //        case _ =>
    //      }
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
    val enzymeNodes = graphDataBaseConnection.findNodes(DynamicLabel.label("Enzyme")).asScala
    val enzymePolys = enzymeNodes.map(
      elem => elem.getRelationships(Direction.INCOMING, BiomeDBRelations.partOf).asScala.map(_.getEndNode).toSet)
//      .map(n => n.getStartNode))
//    enzymePolys
    enzymePolys.zip(enzymeNodes).toMap
  }

  case class ReactionReader(parsedModel: Model)(zipFBCReaction: (org.sbml.jsbml.Reaction, FBCReactionPlugin)) {
    val reactionName = zipFBCReaction._1.getName
    def getPolypeptideByLocusTag(geneProduct: org.sbml.jsbml.ext.fbc.GeneProduct): Option[(String, org.neo4j.graphdb.Node)] = {
      val geneNode = Option(
        graphDataBaseConnection.findNode(
          DynamicLabel.label("Gene"), "locus_tag", geneProduct.getLabel
        )
      )
      val metaIdPolyPair = geneNode match {
        case Some(gn) =>
          val polypeptideNode = gn.getSingleRelationship(BiomeDBRelations.encodes, Direction.OUTGOING).getEndNode
          //        val props = polypeptideNode.getPropertyKeys.asScala.toMap
          Some(geneProduct.getMetaId, polypeptideNode)
        case None => None
      }
      metaIdPolyPair
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

    def makeReactionObject: Reaction = {
      val listOfReactants = zipFBCReaction._1.getListOfReactants.asScala.toList.map(makeReactantObject(_, false))
      val listOfProducts = zipFBCReaction._1.getListOfProducts.asScala.toList.map(makeReactantObject(_, true))
      listOfReactants.foreach(_.upload(graphDataBaseConnection))
      listOfProducts.foreach(_.upload(graphDataBaseConnection))
      //      enzyme needs name of reaction and its node or object
      //      so it can have relationship with it
      //      the result must be a List
      //      so it can be flattened
      //      make a special class for reaction to read it

      //      if (zipFBCReaction._1.getName == "N-Acetyl-D-glucosamine transport via PEP:Pyr PTS  (periplasm)") {
      //        val gpa = zipFBCReaction._2.getGeneProductAssociation.getAssociation
      //        gpa match {
      //          case g: LogicalOperator => //Some(g.getAssociation)
      //            val association = g.getListOfAssociations
      ////            val or = association.asInstanceOf[LogicalOperator]
      //            val assos = g.getListOfAssociations.asScala
      //            println(association)
      //          case _ => None
      //        }
      //      }



      //      listOfGeneAssociations = zipFBCReactions._2.
      //      val listOfReactants = reactantObjects.map(_.upload(graphDataBaseConnection))
      //      val listOfProducts = productObjects.map(_.upload(graphDataBaseConnection))
      val properties = Map("reversible" -> zipFBCReaction._1.isReversible, "metaId" -> zipFBCReaction._1.getMetaId)
      Reaction(name = zipFBCReaction._1.getName, reactants = listOfReactants, products = listOfProducts, properties = properties)
    }

    def getGeneProductsAssociations: List[org.neo4j.graphdb.Node] = {
      val geneAssociations = reactionLoop(zipFBCReaction._2)
      geneAssociations
    }

    def getFBCReaction(fbcReaction: SBasePlugin): Option[FBCReactionPlugin] = {
      val reaction = fbcReaction match {
        case r:FBCReactionPlugin => Some(r)
        case _ => None
      }
      reaction
    }

    def processFBCOperators(operator: LogicalOperator): List[org.neo4j.graphdb.Node] = {//Option[List[org.neo4j.graphdb.Node]]

      val res = operator match {
        case or: Or => processOrOperator(filterOperator(or.getListOfAssociations.asScala.toList))
        case and: And => processAndOperator(filterOperator(and.getListOfAssociations.asScala.toList))
        case _ =>
          logger.error("This is not a LogicalOperator: " + operator)
          List()
      }
      res
    }

    private def filterOperator(operList: List[Association]): List[Association] = {
      val refs = operList.flatMap{
        case fbcReaction: GeneProductRef => Some(fbcReaction)
        case _ => None
      }
      refs
    }

    def processOrOperator(listOfAssociations: List[Association]): List[org.neo4j.graphdb.Node] = {
      //    println(listOfAssociations)
      val res = listOfAssociations match {
        case lgp: List[GeneProductRef] => lgp.map(getOrCreateGeneProduct)
        case _ =>
          logger.error("Not a GeneProductRef: " + listOfAssociations)
          List()
      }
      res
      //    listOfAssociations.map(geneProductCollector(_.getGeneProduct))
    }

    def processAndOperator(listOfAssociations: List[Association]): List[org.neo4j.graphdb.Node] = {
      val res = listOfAssociations match {
        case lgp: List[GeneProductRef] =>
          val polys = lgp.map(getOrCreateGeneProduct).toSet
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

    def getOrCreateGeneProduct(gpa: GeneProductRef): org.neo4j.graphdb.Node = {
      val ref = gpa.getGeneProduct
      if (geneProductCollector.contains(ref)) geneProductCollector(ref)
      else {
        val createdGeneProduct = graphDataBaseConnection.createNode(DynamicLabel.label("Polypeptide"))
        createdGeneProduct.addLabel(DynamicLabel.label("To_check"))
        createdGeneProduct.setProperty("metaId", ref)
        geneProductCollector ++= Map(ref -> createdGeneProduct)
        createdGeneProduct
      }
    }

    //  def processAndOperator(andOperator: And) = {
    //    val xrefs = andOperator.getListOfAssociations.asScala.toList
    //    val refStrings = xrefs.map(_.getCVTerm(0).toString)
    //    val res = refStrings.map(geneProductCollector(_))
    //  }

    def reactionLoop(fbcReaction: FBCReactionPlugin): List[org.neo4j.graphdb.Node] = {//
    val gpa = Option(fbcReaction.getGeneProductAssociation)
      val res = gpa match {
        case Some(g) => //Some(g.getAssociation)
          operatorLoop2(g.getAssociation)
        //        operatorLoop(g.getAssociation)
        //        val association = g.getListOfAssociations
        //        val or = association.asInstanceOf[LogicalOperator]
        //        val assos = or.getListOfAssociations.asScala
        //        println(association)
        case None => List()
      }
      res
    }

//    def operatorLoop[T >: LogicalOperator](operator: T): List[org.neo4j.graphdb.Node] = {
//      val res = operator match {
//        case op: LogicalOperator =>
//          val associations = op.getListOfAssociations.asScala.toList
//          val resOp = associations.head match {
//            case lo: LogicalOperator => associations.map(operatorLoop)
//            case gpr: GeneProductRef => processFBCOperators(op)
//          }
//          resOp
//        case _ =>
//          logger.error("Not a LogicalOperator: " + operator)
//          List()
//      }
//      //      val or = associations.asInstanceOf[LogicalOperator]
//      //      val assos = or.getListOfAssociations.asScala
//      //      println(associations)
//      res
//    }

    def operatorLoop2[T >: LogicalOperator](operator: T): List[org.neo4j.graphdb.Node] = {
      val res = operator match {
        case op: LogicalOperator =>
          val associations = op.getListOfAssociations.asScala.toList
          val operators = associations.flatMap{
            case lo: LogicalOperator => Some(lo)
            case _ => None
          }
          if (operators.nonEmpty) {
            val operatorResults = operators.flatMap(operatorLoop2)
            processFBCOperators(op) ++ operatorResults
          }
          else processFBCOperators(op)
        //      val resOp = associations.head match {
        //        case lo: LogicalOperator => associations.map(operatorLoop)
        //        case gpr: GeneProductRef => processFBCOperators(op)
        //      }
        //      Some(resOp)
        case _ =>
          logger.warn("Not a LogicalOperator: " + operator)
          List()
      }
      //      val or = associations.asInstanceOf[LogicalOperator]
      //      val assos = or.getListOfAssociations.asScala
      //      println(associations)
      res
    }
  }

}
