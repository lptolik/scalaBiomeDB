package BioGraph.sbml

import java.io.File

import org.sbml.jsbml.{Reaction, SBMLReader, Species}
import org.sbml.jsbml.ext.fbc.{FBCModelPlugin, FBCReactionPlugin, FBCSpeciesPlugin}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

/**
  * Created by ramso on 3/5/17.
  */
object CompareModelsApp extends App {
//  val file1 = args(0)
//  val file2 = args(1)
  val file1 = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/sbmls/iECW_1372.xml"
  val file2 = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/sbml_out/out.xml"

  val model1 = new SBMLReader().readSBML(new File(file1)).getModel
  val model2 = new SBMLReader().readSBML(new File(file2)).getModel

  var j = Stream.from(0).iterator
  val shit = HashMap(model1.getListOfReactions.asScala.map(r => (r.getId, j.next)):_*)

  require(model1.getReactionCount == model2.getReactionCount, "different reactions counts")
  require(model1.getSpeciesCount == model2.getSpeciesCount, "different species counts")
  val model1FBC = model1.getPlugin("fbc").asInstanceOf[FBCModelPlugin]
  val model2FBC = model2.getPlugin("fbc").asInstanceOf[FBCModelPlugin]
  require(model1FBC.getGeneProductCount == model2FBC.getGeneProductCount, "different gene products counts")

  val model2ReactionsMap = HashMap(model2.getListOfReactions.asScala.map(r => (r.getId, r)):_*)
  val model2SpeciesMap = HashMap(model2.getListOfSpecies.asScala.map(r => (r.getId, r)):_*)

  val model1ParamsMap = model1.getListOfParameters.asScala.map(p => (p.getId, p.getValue)).toMap
  val model2ParamsMap = model2.getListOfParameters.asScala.map(p => (p.getId, p.getValue)).toMap

  var i = 0

  model1.getListOfReactions.asScala.foreach { r1 =>
    val r2 = model2ReactionsMap(r1.getId)

    compareReactions(r1, r2)
  }
  model1.getListOfSpecies.asScala.foreach { s1 =>
    val s2 = model2SpeciesMap(s1.getId)

    compareSpecies(s1, s2)
  }

  println(s"$i reactions validated")

  def compareReactions(r1: Reaction, r2: Reaction): Unit = {
    val r1FBC = r1.getPlugin("fbc").asInstanceOf[FBCReactionPlugin]
    val r2FBC = r2.getPlugin("fbc").asInstanceOf[FBCReactionPlugin]

    require(r1.getFast == r2.getFast, s"fast of reactions ${r1.getId}")
    require(model1ParamsMap(r1FBC.getUpperFluxBound) == model2ParamsMap(r2FBC.getUpperFluxBound), s"upper flux bound ${r1.getId}")
    require(model1ParamsMap(r1FBC.getLowerFluxBound) == model2ParamsMap(r2FBC.getLowerFluxBound), s"lower flux bound ${r1.getId}")
    require(r1.getReversible == r2.getReversible, "")

    compareReactantsAndProducts(r1, r2)

    i += 1
  }

  def compareReactantsAndProducts(r1: Reaction, r2: Reaction): Unit = {
    require(r1.getReactantCount == r2.getReactantCount , s"different reactants counts ${r1.getId}")
    require(r1.getProductCount == r2.getProductCount, s"different products counts ${r1.getId}")

    val r2ReactantsMap = r2.getListOfReactants.asScala.map(r => (r.getSpecies, r)).toMap
    val r2ProductsMap = r2.getListOfProducts.asScala.map(r => (r.getSpecies, r)).toMap

    r1.getListOfReactants.asScala.foreach { react1 =>
      val react2 = r2ReactantsMap(react1.getSpecies)

      require(react1.getConstant == react2.getConstant, s"constant ${r1.getId} ${react2.getSpecies}")
      require(react1.getStoichiometry == react2.getStoichiometry, s"stoichiometry ${r1.getId} ${react2.getSpecies}")
    }
  }

  def compareSpecies(s1: Species, s2: Species): Unit = {
    val s1FBC = s1.getPlugin("fbc").asInstanceOf[FBCSpeciesPlugin]
    val s2FBC = s2.getPlugin("fbc").asInstanceOf[FBCSpeciesPlugin]

    require(s1.getBoundaryCondition == s2.getBoundaryCondition, s"BoundaryCondition: ${s1.getId}")
    require(s1.getCompartment == s2.getCompartment, s"Compartment: ${s1.getId}")
    require(s1.getConstant == s2.getConstant, s"Constant: ${s1.getId}")
    require(s1FBC.getCharge == s2FBC.getCharge, s"Charge: ${s1.getId}")
    require(s1FBC.getChemicalFormula == s2FBC.getChemicalFormula, s"ChemicalFormula: ${s1.getId}")
    require(s1.getHasOnlySubstanceUnits == s2.getHasOnlySubstanceUnits, s"HasOnlySubstanceUnits: ${s1.getId}")
  }
}
