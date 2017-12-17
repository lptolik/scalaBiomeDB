package BioGraph.sbml

import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb._
import org.sbml.jsbml._
import org.sbml.jsbml.ext.fbc._
import utilFunctions.BiomeDBRelations._
import utilFunctions.{BiomeDBRelations, TransactionSupport}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Try

/**
  * Created by piane_ramso on 12/16/16.
  */
object JSBMLExport extends TransactionSupport {
  private val logger = LogManager.getLogger(this.getClass.getName)
  val defaultNil = 0
  val defaultLowerBound = -1000
  val defaultUpperBound = 1000

  def assembleExistingModel(organismName: String, modelName: String)(db: GraphDatabaseService): SBMLDocument = {
    val cypher =
      s"MATCH (o:Organism {name: '$organismName'})<-[:${BiomeDBRelations.partOf}]-(br:BiochemicalReaction) " +
        s"RETURN br"

    val organismReactionNodes = db.execute(cypher).columnAs[Node]("br").asScala.toList

    assembleModel(organismReactionNodes, modelName)(db)
  }

  def assembleHomologyModel(targetOrganismName: String, modelName: String, biomassReactionId: String)
                           (db: GraphDatabaseService): SBMLDocument = {

    val (referenceOrganismName, referenceOrganismReactions) = getReferenceModelReactions(targetOrganismName)(db) match {
      case Some(r) => r
      case None =>
        throw new Exception(s"There is no similar organism for '$targetOrganismName', can't assemble model")
    }

    val qSimilar = s"MATCH (:Organism {name: '$targetOrganismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
      s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)-[:${BiomeDBRelations.similar.name()}]-" +
      s"(:AA_Sequence)<-[:${BiomeDBRelations.isA.name()}]-(:Polypeptide)-[:${BiomeDBRelations.catalyzes.name()}]->" +
      s"(brs:BiochemicalReaction) " +
      s"WHERE NOT (brs)-[:PART_OF]->(:Organism {name: '$referenceOrganismName'})" +
      s"RETURN DISTINCT brs"

    val qSame = s"MATCH (:Organism {name: '$targetOrganismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
      s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)" +
      s"<-[:${BiomeDBRelations.isA.name()}]-(:Polypeptide)-[:${BiomeDBRelations.catalyzes.name()}]->" +
      s"(brs:BiochemicalReaction)" +
      s"WHERE NOT (brs)-[:PART_OF]->(:Organism {name: '$referenceOrganismName'})" +
      s"RETURN DISTINCT brs"

    //    val qBiomass = s"MATCH (n:BiochemicalReaction {sbmlId: '$biomassReactionId'}) RETURN n"

    val qSpontaneous = s"MATCH (s:SpontaneousReaction) RETURN s"

    val qNoprotein = s"MATCH (n:BiochemicalReaction)" +
      s"WHERE NOT (n)<-[:${BiomeDBRelations.catalyzes.name()}]-() RETURN n"

    val sameReactionsNodes = db.execute(qSame).columnAs[Node]("brs").asScala.toSeq
    val similarReactionsNodes = db.execute(qSimilar).columnAs[Node]("brs").asScala.toSeq
    val spontaneousReactionNodes = db.execute(qSpontaneous).columnAs[Node]("s").asScala.toSeq
    val noProteinReactionNodes = db.execute(qNoprotein).columnAs[Node]("n").asScala.toSeq
    val complexesReactions = getComplexesReactionsByHomology(targetOrganismName, referenceOrganismName)(db)

    val otherOrganismsReactions = (sameReactionsNodes ++
      similarReactionsNodes ++
      spontaneousReactionNodes ++
      noProteinReactionNodes ++
      complexesReactions).distinct

    assembleModel(referenceOrganismReactions ++ otherOrganismsReactions, modelName)(db)
  }

  def getReferenceModelReactions(targetOrganismName: String)(db: GraphDatabaseService): Option[(String, Seq[Node])] = {
    val qReferenceOrganism = s"MATCH (o1:Organism { name: '$targetOrganismName' })<-[:PART_OF]-" +
      s"(:Polypeptide)-[:IS_A]->(:AA_Sequence)<-[:IS_A]-(p2:Polypeptide)-[:PART_OF]->(o2:Organism)<-[PART_OF]-(m:Model) " +
      s"WHERE o1 <> o2 RETURN COUNT(p2) as sameProts, o2.name ORDER BY sameProts DESC LIMIT 1"

    db.execute(qReferenceOrganism).columnAs[String]("o2.name").asScala.toSeq.headOption.map { referenceOrganismName =>
      logger.info(s"Reference organism for '$targetOrganismName' model is '$referenceOrganismName'")
      println(s"Reference organism for '$targetOrganismName' model is '$referenceOrganismName'")

      val qSimilar = s"MATCH (:Organism {name: '$targetOrganismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
        s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)-[:${BiomeDBRelations.similar.name()}]-" +
        s"(:AA_Sequence)<-[:${BiomeDBRelations.isA.name()}]-(:Polypeptide)-[:${BiomeDBRelations.catalyzes.name()}]->" +
        s"(brs:BiochemicalReaction)-[:PART_OF]->(:Organism {name: '$referenceOrganismName'})" +
        s"RETURN DISTINCT brs"

      val qSame = s"MATCH (:Organism {name: '$targetOrganismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
        s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)" +
        s"<-[:${BiomeDBRelations.isA.name()}]-(:Polypeptide)-[:${BiomeDBRelations.catalyzes.name()}]->" +
        s"(brs:BiochemicalReaction)-[:PART_OF]->(:Organism {name: '$referenceOrganismName'})" +
        s"RETURN DISTINCT brs"

      val sameReactionsNodes = db.execute(qSame).columnAs[Node]("brs").asScala.toSeq.distinct
      val similarReactionsNodes = db.execute(qSimilar).columnAs[Node]("brs").asScala.toSeq.distinct
      val complexReactionsNodes = getComplexesReactionsFromSourceOrganismByHomology(
        targetOrganismName, referenceOrganismName)(db)

      logger.info(s"Identical proteins with reactions count: ${sameReactionsNodes.size}")
      logger.info(s"Similar proteins with reactions count: ${similarReactionsNodes.size}")
      logger.info(s"Complexes with reactions count: ${complexReactionsNodes.size}")
      println(s"Identical proteins with reactions count: ${sameReactionsNodes.size}")
      println(s"Similar proteins with reactions count: ${similarReactionsNodes.size}")
      println(s"Complexes with reactions count: ${complexReactionsNodes.size}")

      (referenceOrganismName, sameReactionsNodes ++ similarReactionsNodes ++ complexReactionsNodes)
    }
  }

  private def getComplexesReactionsByHomology(organismName: String, exceptOrganismName: String)
                                             (db: GraphDatabaseService): Iterable[Node] = {

    val qSimilar = s"MATCH (:Organism {name: '$organismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
      s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)-[:${BiomeDBRelations.similar.name()}]-" +
      s"(:AA_Sequence)<-[:${BiomeDBRelations.isA.name()}]-(p:Polypeptide)" +
      s"RETURN DISTINCT p"

    val qSame = s"MATCH (:Organism {name: '$organismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
      s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)" +
      s"<-[:${BiomeDBRelations.isA.name()}]-(p:Polypeptide)" +
      s"RETURN DISTINCT p"

    val sourceHomologousPolypeptidesIds = (db.execute(qSame).columnAs[Node]("p").asScala ++
      db.execute(qSimilar).columnAs[Node]("p").asScala).map(_.getId).toSeq

    getComplexesReactionsNodes(exceptOrganismName)(db).flatMap { complexReaction =>
      if (complexReaction.complexPolypeptidesIds.forall(sourceHomologousPolypeptidesIds.contains))
        Some(complexReaction.reaction)
      else
        None
    }
  }

  private def getComplexesReactionsFromSourceOrganismByHomology(targetOrganismName: String, sourceOrganismName: String)
                                                               (db: GraphDatabaseService)
  : Iterable[Node] = {

    val qSimilar = s"MATCH (:Organism {name: '$targetOrganismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
      s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)-[:${BiomeDBRelations.similar.name()}]-" +
      s"(:AA_Sequence)<-[:${BiomeDBRelations.isA.name()}]-(p:Polypeptide)" +
      s"-[:PART_OF]->(:Organism {name: '$sourceOrganismName'})" +
      s"RETURN DISTINCT p"

    val qSame = s"MATCH (:Organism {name: '$targetOrganismName'})<-[:${BiomeDBRelations.partOf.name()}]" +
      s"-(:Polypeptide)-[:${BiomeDBRelations.isA.name()}]->(:AA_Sequence)" +
      s"<-[:${BiomeDBRelations.isA.name()}]-(p:Polypeptide)" +
      s"-[:PART_OF]->(:Organism {name: '$sourceOrganismName'})" +
      s"RETURN DISTINCT p"

    val sourceHomologousPolypeptidesIds = (db.execute(qSame).columnAs[Node]("p").asScala ++
      db.execute(qSimilar).columnAs[Node]("p").asScala).map(_.getId).toSeq

    getComplexesReactionsFromSourceNodes(sourceOrganismName)(db).flatMap { complexReaction =>
      if (complexReaction.complexPolypeptidesIds.forall(sourceHomologousPolypeptidesIds.contains))
        Some(complexReaction.reaction)
      else
        None
    }
  }

  private def getComplexesReactionsNodes(exceptOrganismName: String)
                                        (db: GraphDatabaseService): Iterable[ComplexBiochemicalReaction] = {

    val q = "MATCH (brs:BiochemicalReaction)<-[:CATALYZES]-(e:Enzyme)<-[:PART_OF]-(p:Polypeptide) " +
      s"WHERE NOT (brs)-[:PART_OF]->(:Organism {name: '$exceptOrganismName'})" +
      "RETURN brs, p"
    val qResult = db
      .execute(q)

    (qResult.columnAs[Node]("brs").asScala.toSeq zip qResult.columnAs[Node]("p").asScala.toSeq)
      .groupBy(_._1)
      .map { case (r, rsps) =>
        ComplexBiochemicalReaction(rsps.map(_._2.getId), r)
      }.toSet
  }
  private def getComplexesReactionsFromSourceNodes(sourceOrganismName: String)(db: GraphDatabaseService)
  : Iterable[ComplexBiochemicalReaction] = {

    val qResult = db
      .execute(s"MATCH (:Organism {name: '$sourceOrganismName'})<-[:PART_OF]-" +
        "(r:BiochemicalReaction)<-[:CATALYZES]-(e:Enzyme)<-[:PART_OF]-(p:Polypeptide) return r, p")

    (qResult.columnAs[Node]("r").asScala.toSeq zip qResult.columnAs[Node]("p").asScala.toSeq)
      .groupBy(_._1)
      .map { case (r, rsps) =>
        ComplexBiochemicalReaction(rsps.map(_._2.getId), r)
      }.toSet
  }

  case class ComplexBiochemicalReaction(complexPolypeptidesIds: Seq[Long], reaction: Node)

  def assembleModel(reactionsNodes: Seq[Node], modelName: String)
                   (db: GraphDatabaseService): SBMLDocument = {

    val sbmlDoc = new SBMLDocument(3, 1)
    val model = sbmlDoc.createModel(modelName)

    val reactions = getReactionsOut(reactionsNodes)
    val species = reactions.flatMap(r => r._1.products.map(_.speciesOut) ++ r._1.reactants.map(_.speciesOut)).distinct
    val geneProducts = reactions.flatMap(_._2.allGeneProducts).distinct

    val compartmentNameToCompartment = addCompartments(species, model)
    val speciesToSbmlSpecies = addSpecies(species, model, compartmentNameToCompartment)

    val modelFBC = model.getPlugin("fbc").asInstanceOf[FBCModelPlugin]
    modelFBC.setStrict(true)
    addListOfUnits(model)
    val defaultParameters = addDefaultParameters(model)
    val geneProductToSBMLGeneProduct = addGeneProducts(geneProducts, modelFBC)

    addReactions(reactions, model, speciesToSbmlSpecies, geneProductToSBMLGeneProduct, defaultParameters)

    sbmlDoc
  }

  def writeToFile(sbmlDoc: SBMLDocument, filename: String) = {
    SBMLWriter.write(sbmlDoc, filename, "biograph", "0.9")
  }

  private def addListOfUnits(model: Model) = {
    val unitDefinition = new UnitDefinition("mmol_per_gDW_per_hr", 3, 1)

    val unitMoles = new Unit(1, -3, Unit.Kind.MOLE, 1, 3, 1)
    val unitGrams = new Unit(1, 0, Unit.Kind.GRAM, -1, 3, 1)
    val unitSeconds = new Unit(3600, 0, Unit.Kind.SECOND, -1, 3, 1)

    unitDefinition.addUnit(unitMoles)
    unitDefinition.addUnit(unitGrams)
    unitDefinition.addUnit(unitSeconds)

    model.addUnitDefinition(unitDefinition)
  }

  case class ModelParameters(private val defaultParams: Seq[Parameter], private val model: Model) {

    private val params = mutable.HashMap(defaultParams.map(p => (p.getValue, p)):_*)
    def getOrAdd(key: Double): Parameter = params.getOrElse(key, {
      val name = s"Biome DB param ${convertToString(key)}"
      val id = idFromName(name)
      val param = addParameter(model, id, name, key)
      params.put(key, param)
      param
    })

    private def convertToString(value: Double): String = {
      if (value < 0)
        s"minus ${scala.math.abs(value).toString}"
      else
        value.toString
    }

    private def idFromName(id: String): String = {
      id.replace(" ", "_").replace("-", "_").replace(".", "_")
    }
  }
  private def addDefaultParameters(model: Model): ModelParameters = {
    val parUpper = addParameter(model, "Biome_DB_upper_bound", "Biome DB upper bound", defaultUpperBound)
    val parLower = addParameter(model, "Biome_DB_lower_bound", "Biome DB lower bound", defaultLowerBound)
    val parNil = addParameter(model, "Biome_DB_nil_value", "Biome DB nil value", defaultNil)

    ModelParameters(Seq(parLower, parUpper, parNil), model)
  }

  def addParameter(model: Model, id: String, name: String, value: Double, units: String = "mmol_per_gDW_per_hr") = {
    val parUpper = new Parameter(id, 3, 1)
    parUpper.setConstant(true)
    parUpper.setName(name)
    parUpper.setSBOTerm(626)
    parUpper.setUnits(units)
    parUpper.setValue(value)
    model.addParameter(parUpper)
    parUpper
  }

  private def addGeneProducts(geneProducts: Seq[GeneProductOut], modelFBC: FBCModelPlugin) = {
    geneProducts.map { gp =>
      val sbmlGP = new GeneProduct(gp.sbmlId, 3, 1)
      sbmlGP.setId(gp.sbmlId)
      sbmlGP.setMetaId(gp.metaId)
      sbmlGP.setName(gp.name)
      sbmlGP.setLabel(gp.label)

      if (modelFBC.getGeneProduct(gp.sbmlId) == null)
        modelFBC.addGeneProduct(sbmlGP)
      else {
        val nextNumber = Try(gp.sbmlId.substring(gp.sbmlId.length - 1, gp.sbmlId.length).toInt).toOption.getOrElse(0) + 1
        val newId = gp.sbmlId.take(gp.sbmlId.length - 1) + nextNumber.toString
        sbmlGP.setId(newId)
        sbmlGP.setMetaId(newId)

        modelFBC.addGeneProduct(sbmlGP)
      }
      (gp, sbmlGP)
    }.toMap
  }

  private def addReactions(reactions: Seq[(ReactionOut, GeneProductAssociationOut)],
                           model: Model,
                           speciesToSbmlSpecies: Map[SpeciesOut, Species],
                           geneProductToSBMLGeneProduct: Map[GeneProductOut, GeneProduct],
                           params: ModelParameters) = {

    def getParam(value: Double) = params.getOrAdd(value)

    val added = new mutable.HashSet[String]()

    reactions.foreach { case (reaction, gpa) =>
      if (!added.contains(reaction.sbmlId)) {
        val sbmlR = new Reaction(reaction.metaId, 3, 1)
        if (reaction.metaId.nonEmpty) {
          sbmlR.setMetaId(reaction.metaId)
        }
        sbmlR.setId(reaction.sbmlId)
        sbmlR.setName(reaction.name)
        sbmlR.setReversible(reaction.reversible)
        sbmlR.setFast(false)

        val reactionFBC = sbmlR.getPlugin("fbc").asInstanceOf[FBCReactionPlugin]

        reactionFBC.setUpperFluxBound(getParam(reaction.upperFluxBound))
        reactionFBC.setLowerFluxBound(getParam(reaction.lowerFluxBound))

        gpa match {
          case GeneProductRefOut(geneProduct) =>
            val sbmlGPA = new GeneProductAssociation(3, 1)
            val association: GeneProductRef = getRefAssociation(geneProduct)
            sbmlGPA.setAssociation(association)
            reactionFBC.setGeneProductAssociation(sbmlGPA)

          case OrOut(geneProducts) =>
            val sbmlGPA = new GeneProductAssociation(3, 1)
            val association = new Or(3, 1)
            geneProducts.foreach {
              case GeneProductRefOut(gp) =>
                val innerAssociation: GeneProductRef = getRefAssociation(gp)
                association.addAssociation(innerAssociation)
              case AndOut(gps) =>
                val innerAssociation: And = getAndAssociation(gps)
                association.addAssociation(innerAssociation)
            }
            sbmlGPA.setAssociation(association)
            reactionFBC.setGeneProductAssociation(sbmlGPA)

          case AndOut(geneProducts) =>
            val sbmlGPA = new GeneProductAssociation(3, 1)
            val association: And = getAndAssociation(geneProducts)
            sbmlGPA.setAssociation(association)
            reactionFBC.setGeneProductAssociation(sbmlGPA)

          case NoAssociations =>
        }

        reaction.reactants.foreach { r =>
          val sr = new SpeciesReference(speciesToSbmlSpecies(r.speciesOut))
          sr.setStoichiometry(r.stoichiometry)
          sr.setConstant(true)
          sbmlR.addReactant(sr)
        }
        reaction.products.foreach { r =>
          val sr = new SpeciesReference(speciesToSbmlSpecies(r.speciesOut))
          sr.setStoichiometry(r.stoichiometry)
          sr.setConstant(true)
          sbmlR.addProduct(sr)
        }

        model.addReaction(sbmlR)
        added.add(reaction.sbmlId)
      } else {
        val a = 0
      }
    }

    def getRefAssociation(geneProduct: GeneProductOut) = {
      val association = new GeneProductRef(3, 1)
      association.setGeneProduct(geneProduct.sbmlId)
      association
    }

    def getAndAssociation(geneProducts: Seq[GeneProductRefOut]) = {
      val association = new And(3, 1)
      geneProducts.foreach { gp =>
        val innerAssociation = new GeneProductRef(3, 1)
        innerAssociation.setGeneProduct(gp.geneProduct.sbmlId)
        association.addAssociation(innerAssociation)
      }
      association
    }
  }

  private def addSpecies(species: Seq[SpeciesOut],
                         model: Model,
                         compartmentNameToCompartment: Map[String, Compartment]) = {

    val added = new mutable.HashMap[String, Species]()

    species.map { s =>
      added.get(s.sbmlId).map(sbmlS => (s, sbmlS)).getOrElse {
        val sbmlS = new Species(s.sbmlId, 3, 1)
        sbmlS.setMetaId(s.metaId)
        sbmlS.setName(s.name)
        sbmlS.setConstant(false)
        sbmlS.setCompartment(compartmentNameToCompartment(s.compartment))
        sbmlS.setHasOnlySubstanceUnits(false)
        sbmlS.setBoundaryCondition(false)
        sbmlS.setSBOTerm(s.sboTerm)
        val speciesFBC = sbmlS.getPlugin("fbc").asInstanceOf[FBCSpeciesPlugin]
        s.charge.foreach(ch => speciesFBC.setCharge(ch))
        speciesFBC.setChemicalFormula(s.chemicalFormula)

        model.addSpecies(sbmlS)
        added.put(s.sbmlId, sbmlS)

        (s, sbmlS)
      }
    }.toMap
  }

  private def addCompartments(species: Seq[SpeciesOut], model: Model) = {
    species
      .map(_.compartment)
      .distinct
      .map { cn =>
        val compartment = new Compartment(cn.take(1), 3, 1)
        compartment.setName(cn)
        compartment.setConstant(true)
        model.addCompartment(compartment)
        (cn, compartment)
      }.toMap
  }

  private def getReactionsOut(reactionsNodes: Seq[Node]): Seq[(ReactionOut, GeneProductAssociationOut)] = {
    reactionsNodes
      .map { r =>
        val reactants = r.getRelationships(is_reactant).asScala.map { r =>
          val s = SpeciesOut(r.getStartNode)
          val stoichiometry = r.getProperty("stoichiometric_coef").toString.toDouble
          ReactantOut(s, -stoichiometry)
        }
        val products = r.getRelationships(is_product).asScala.map { r =>
          val s = SpeciesOut(r.getEndNode)
          val stoichiometry = r.getProperty("stoichiometric_coef").toString.toDouble
          ReactantOut(s, stoichiometry)
        }

        (ReactionOut(r, reactants, products), GeneProductAssociationOut(r))
      }
  }
}

case class ReactionOut(sbmlId: String,
                       metaId: String,
                       name: String,
                       reversible: Boolean,
                       experiment: String,
                       reactants: Iterable[ReactantOut],
                       products: Iterable[ReactantOut],
                       lowerFluxBound: Double,
                       upperFluxBound: Double)
object ReactionOut {
  def apply(reactionNode: Node, reactants: Iterable[ReactantOut], products: Iterable[ReactantOut]): ReactionOut = {
    val props = reactionNode.getAllProperties
    val getProp = (k: String) => props.getOrDefault(k, "").toString

    ReactionOut(getProp("sbmlId"), getProp("metaId"), getProp("name"), getProp("reversible").toBoolean,
      getProp("experiment"), reactants, products,
      getProp("lowerFluxBound").toDouble, getProp("upperFluxBound").toDouble)
  }
}

case class SpeciesOut(sbmlId: String,
                      metaId: String,
                      name: String,
                      chemicalFormula: String,
                      compartment: String,
                      charge: Option[Int],
                      sboTerm: Int)
case class ReactantOut(speciesOut: SpeciesOut, stoichiometry: Double)
object SpeciesOut {
  def apply(reactantNode: Node): SpeciesOut = {
    val props = reactantNode.getAllProperties
    val getProp = (k: String) => props.getOrDefault(k, "").toString
    val compartment = reactantNode.getRelationships(locates_in).asScala.head.getEndNode.getProperty("name").toString

    SpeciesOut(getProp("sbmlId"), getProp("metaId"), getProp("name"), getProp("chemical_formula"), compartment,
      Try(getProp("charge").toInt).toOption, getProp("sboTerm").toInt)
  }
}

case class GeneProductOut(sbmlId: String,
                          label: String,
                          name: String,
                          metaId: String)
object GeneProductOut {
  def apply(polypeptideNode: Node): GeneProductOut = {
    val props = polypeptideNode.getAllProperties
    val name = props.getOrDefault("name", "").toString
    val locusTag = polypeptideNode
      .getRelationships(encodes)
      .asScala
      .headOption
      .map(r => r.getStartNode.getProperty("locus_tag").toString)
      .getOrElse(props.getOrDefault("label", "").toString)
    val sbmlId = "G_" + locusTag

    GeneProductOut(sbmlId, locusTag, name, sbmlId)
  }
}

sealed trait GeneProductAssociationOut {
  val allGeneProducts: List[GeneProductOut]
}
case class GeneProductRefOut(geneProduct: GeneProductOut) extends GeneProductAssociationOut {
  override val allGeneProducts: List[GeneProductOut] = List(geneProduct)
}
case class AndOut(members: List[GeneProductRefOut]) extends GeneProductAssociationOut {
  override val allGeneProducts: List[GeneProductOut] = members.map(_.geneProduct)
}
case class OrOut(members: List[GeneProductAssociationOut]) extends GeneProductAssociationOut {
  override val allGeneProducts: List[GeneProductOut] = members.flatMap(_.allGeneProducts)
}
case object NoAssociations extends GeneProductAssociationOut {
  override val allGeneProducts: List[GeneProductOut] = Nil
}

object GeneProductAssociationOut {
  val polypeptideLabel = DynamicLabel.label("Polypeptide")
  val enzymeLabel = DynamicLabel.label("Enzyme")

  def apply(reactionNode: Node): GeneProductAssociationOut = {
    reactionNode.getRelationships(catalyzes).asScala.map(_.getStartNode).toList match {

      case geneProductNode :: Nil if hasLabel(geneProductNode, polypeptideLabel) =>
        GeneProductRefOut(GeneProductOut(geneProductNode))

      case geneProductNode :: Nil if hasLabel(geneProductNode, enzymeLabel) =>
        val geneProducts = getComplexComponents(geneProductNode)
        AndOut(geneProducts)

      case Nil => NoAssociations

      case geneProductsNodes: List[Node] =>
        val geneProducts = geneProductsNodes.map {
          case gpn if hasLabel(gpn, polypeptideLabel) => GeneProductRefOut(GeneProductOut(gpn))
          case gpn if hasLabel(gpn, enzymeLabel) => AndOut(getComplexComponents(gpn))
        }
        OrOut(geneProducts)
    }
  }

  private def getComplexComponents(geneProductNode: Node) = {
    geneProductNode
      .getRelationships(partOf)
      .asScala
      .toList
      .map(r => GeneProductRefOut(GeneProductOut(r.getStartNode)))
  }

  private def hasLabel(node: Node, label: Label): Boolean = {
    node.getLabels.asScala.toList.contains(label)
  }
}
