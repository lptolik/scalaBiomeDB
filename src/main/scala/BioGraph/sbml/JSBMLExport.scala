package BioGraph.sbml

import java.io.File

import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.{DynamicLabel, Label, Node}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml._
import org.sbml.jsbml.ext.fbc._
import utilFunctions.BiomeDBRelations._
import utilFunctions.TransactionSupport

import scala.collection.JavaConverters._
import scala.collection.immutable.::

/**
  * Created by piane_ramso on 12/16/16.
  */
object JSBMLExport extends App with TransactionSupport {
  val logger = LogManager.getLogger(this.getClass.getName)

  val localDB = new File("/Users/piane_ramso/Ya.Disk/Yandex.Disk/Studying/PhD/thesis/pushchono_phd/data/graph.db")
  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)

  val reactionLabel = DynamicLabel.label("Reaction")
  val defaultNil = 0
  val defaultLowerBound = -1000
  val defaultUpperBound = 1000

  transaction(graphDataBaseConnection) {

    val reactions = getReactions()
      .toList
    val species = reactions.flatMap(r => r._1.products.map(_.speciesOut) ++ r._1.reactants.map(_.speciesOut)).distinct
    val geneProducts = reactions.flatMap(_._2.allGeneProducts).distinct

    val sbmlDoc = new SBMLDocument(3, 1)
    val model = sbmlDoc.createModel("biograph_export_model")

    val compartmentNameToCompartment = addCompartments(species, model)
    val speciesToSbmlSpecies = addSpecies(species, model, compartmentNameToCompartment)

    val modelFBC = model.getPlugin("fbc").asInstanceOf[FBCModelPlugin]
    modelFBC.setStrict(true)
    addListOfUnits(model)
    val defaultParameters = addParameters(model)
    val geneProductToSBMLGeneProduct = addGeneProducts(geneProducts, modelFBC)

    addReactions(reactions, model, speciesToSbmlSpecies, geneProductToSBMLGeneProduct, defaultParameters)

    val out = "/Users/piane_ramso/Ya.Disk/Yandex.Disk/Studying/PhD/thesis/pushchono_phd/sbml_out/out.xml"
    SBMLWriter.write(sbmlDoc, out, "biograph", "0.9")
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

  case class DefaultParameters(lowerBound: Parameter, upperBound: Parameter, nil: Parameter)
  private def addParameters(model: Model): DefaultParameters = {
    val parUpper = new Parameter("default_ub", 3, 1)
    parUpper.setConstant(true)
    parUpper.setName("default upper bound")
    parUpper.setSBOTerm(626)
    parUpper.setUnits("mmol_per_gDW_per_hr")
    parUpper.setValue(defaultUpperBound)
    val parLower = new Parameter("default_lb", 3, 1)
    parLower.setConstant(true)
    parLower.setName("default lower bound")
    parLower.setSBOTerm(626)
    parLower.setUnits("mmol_per_gDW_per_hr")
    parLower.setValue(defaultLowerBound)
    val parNil = new Parameter("nil", 3, 1)
    parNil.setConstant(true)
    parNil.setName("nil value")
    parNil.setSBOTerm(626)
    parNil.setUnits("mmol_per_gDW_per_hr")
    parNil.setValue(defaultNil)

    model.addParameter(parUpper)
    model.addParameter(parLower)
    model.addParameter(parNil)

    DefaultParameters(parLower, parUpper, parNil)
  }

  private def addGeneProducts(geneProducts: List[GeneProductOut], modelFBC: FBCModelPlugin) = {
    geneProducts.map { gp =>
      val sbmlGP = new GeneProduct(gp.sbmlId, 3, 1)
      sbmlGP.setId(gp.sbmlId)
      sbmlGP.setMetaId(gp.metaId)
      sbmlGP.setName(gp.name)
      sbmlGP.setLabel(gp.label)

      modelFBC.addGeneProduct(sbmlGP)
      (gp, sbmlGP)
    }.toMap
  }

  private def addReactions(reactions: List[(ReactionOut, GeneProductAssociationOut)],
                           model: Model,
                           speciesToSbmlSpecies: Map[SpeciesOut, Species],
                           geneProductToSBMLGeneProduct: Map[GeneProductOut, GeneProduct],
                           defaultParameters: DefaultParameters) = {

    reactions.foreach { case (r, gpa) =>
      val sbmlR = new Reaction(r.metaId, 3, 1)
      if (r.metaId.nonEmpty) {
        sbmlR.setMetaId(r.metaId)
      }
      sbmlR.setId(r.sbmlId)
      sbmlR.setName(r.name)
      sbmlR.setReversible(r.reversible)
      sbmlR.setFast(false)

      val reactionFBC = sbmlR.getPlugin("fbc").asInstanceOf[FBCReactionPlugin]
      reactionFBC.setUpperFluxBound(defaultParameters.upperBound)
      if (r.reversible)
        reactionFBC.setLowerFluxBound(defaultParameters.lowerBound)
      else
        reactionFBC.setLowerFluxBound(defaultParameters.nil)

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

      r.reactants.foreach { r =>
        val sr = new SpeciesReference(speciesToSbmlSpecies(r.speciesOut))
        sr.setStoichiometry(r.stoichiometry)
        sr.setConstant(true)
        sbmlR.addReactant(sr)
      }
      r.products.foreach { r =>
        val sr = new SpeciesReference(speciesToSbmlSpecies(r.speciesOut))
        sr.setStoichiometry(r.stoichiometry)
        sr.setConstant(true)
        sbmlR.addProduct(sr)
      }

      model.addReaction(sbmlR)
      sbmlR
    }

    def getRefAssociation(geneProduct: GeneProductOut) = {
      val association = new GeneProductRef(3, 1)
      association.setGeneProduct(geneProduct.sbmlId)
      association
    }

    def getAndAssociation(geneProducts: List[GeneProductRefOut]) = {
      val association = new And(3, 1)
      geneProducts.foreach { gp =>
        val innerAssociation = new GeneProductRef(3, 1)
        innerAssociation.setGeneProduct(gp.geneProduct.sbmlId)
        association.addAssociation(innerAssociation)
      }
      association
    }
  }

  private def addSpecies(species: List[SpeciesOut],
                         model: Model,
                         compartmentNameToCompartment: Map[String, Compartment]) = {
    species.map { s =>
      val sbmlS = new Species(s.metaId, 3, 1)
      sbmlS.setMetaId(s.metaId)
      sbmlS.setName(s.name)
      sbmlS.setConstant(false)
      sbmlS.setCompartment(compartmentNameToCompartment(s.compartment))
      sbmlS.setHasOnlySubstanceUnits(false)
      sbmlS.setBoundaryCondition(false)
      sbmlS.setSBOTerm(s.sboTerm)
      val speciesFBC = sbmlS.getPlugin("fbc").asInstanceOf[FBCSpeciesPlugin]
      speciesFBC.setCharge(s.charge)
      speciesFBC.setChemicalFormula(s.chemicalFormula)

      model.addSpecies(sbmlS)
      (s, sbmlS)
    }.toMap
  }

  private def addCompartments(species: List[SpeciesOut], model: Model) = {
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

  private def getReactions(): Iterator[(ReactionOut, GeneProductAssociationOut)] = {
    graphDataBaseConnection
      .findNodes(reactionLabel).asScala
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
                       products: Iterable[ReactantOut])
object ReactionOut {
  def apply(reactionNode: Node, reactants: Iterable[ReactantOut], products: Iterable[ReactantOut]): ReactionOut = {
    val props = reactionNode.getAllProperties
    val getProp = (k: String) => props.getOrDefault(k, "").toString

    ReactionOut(getProp("sbmlId"), getProp("metaId"), getProp("name"), getProp("reversible").toBoolean, getProp("experiment"),
      reactants, products)
  }
}

case class SpeciesOut(metaId: String,
                      name: String,
                      chemicalFormula: String,
                      compartment: String,
                      charge: Int,
                      sboTerm: Int)
case class ReactantOut(speciesOut: SpeciesOut, stoichiometry: Double)
object SpeciesOut {
  def apply(reactantNode: Node): SpeciesOut = {
    val props = reactantNode.getAllProperties
    val getProp = (k: String) => props.getOrDefault(k, "").toString
    val compartment = reactantNode.getRelationships(locates_in).asScala.head.getEndNode.getProperty("name").toString

    SpeciesOut(getProp("metaId"), getProp("name"), getProp("chemical_formula"), compartment,
      getProp("charge").toInt, getProp("sboTerm").toInt)
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
