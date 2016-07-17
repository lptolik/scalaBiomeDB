package BioGraph

import java.io.File

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml.xml.parsers.AnnotationReader
import org.sbml.jsbml.{Model, SpeciesReference, ModifierSpeciesReference, Species, FunctionDefinition, SBMLReader, Compartment, Reaction, Parameter, Rule}
import utilFunctions.{TransactionSupport, BiomeDBRelations}
import org.neo4j.graphdb.Node

import scala.collection.JavaConverters._

/**
  * Created by artem on 14.07.16.
  */
class JSBMLUtil(dataBaseFile: File) extends TransactionSupport {
  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  val reader = new SBMLReader()
  var compartmentNodes = Map[String, Compartment]()

  def processSBMLFile(fileName: String): Model = transaction(graphDataBaseConnection){
//    get one model
    val openFile = new File(fileName)
    val readResult = reader.readSBML(openFile)
    val model = readResult.getModel

//    val reactions = model.getListOfReactions.asScala.toList
    val compartments = model.getListOfCompartments.asScala.toList
    compartmentNodes = compartments.map(c => c.getName -> new Compartment(c.getName)).toMap
//    val definitions = model.getListOfFunctionDefinitions.asScala.toList
//    val parameters = model.getListOfParameters.asScala.toList
//    val rules = model.getListOfRules.asScala.toList
//    val species = model.getListOfSpecies.asScala.toList
//    val modifiers = model.getModifierSpeciesReferences.asScala.toList


//    val parsedModel = new SBMLModel(
//      reactions,
//      compartments,
//      definitions,
//      parameters,
//      rules,
//      species,
//      modifiers
//    )
////    parsedModel
    model
  }

  def uploadModels(parsedModel: Model) = transaction(graphDataBaseConnection) {
//    upload one model
    //    println("reactions " + reactions)
    //    println("compartments " + compartments)
    //    println("definitions " + definitions)
    //    println("parameters " + parameters)
    //    println("rules " + rules)
    //    println("species " + species)
    //    println("modifiers " + modifiers)
    //    species.foreach(s => println(s))
//    val reactions = parsedModel.getReaction
//    reactions.foreach(processReaction(_, parsedModel))
//    val reaction = reactions.tail.head
//    println(reaction.getListOfReactants.asScala.toList.head)
//    reactions.foreach(r => println(r.getListOfReactants.asScala.toList))
//    processReaction(reaction)
    val reactions = parsedModel.getListOfReactions.asScala.toList

    def makeReactantObject(reactant: SpeciesReference): Reactant = {
      val specie = parsedModel.getSpecies(reactant.getSpecies)
      val name = specie.getName
      val compartment = specie.getCompartment
      val stoi = reactant.getStoichiometry
      new Reactant(name = name, stoichiometry = Some(stoi), compartment = Some(compartmentNodes(specie.getCompartment)))
    }

    def makeReactionObject(reaction: org.sbml.jsbml.Reaction): Reaction = {
      val reactants = reaction.getListOfReactants.asScala.toList
      val m = parsedModel.getModel
      val reactantObjects = reactants.map(makeReactantObject)
      reactantObjects.foreach(_.upload(graphDataBaseConnection))
      new Reaction(reaction.getName, reactantObjects)
    }

    val reactionObjects = reactions.map(makeReactionObject)
    reactionObjects.map(_.upload(graphDataBaseConnection))
//    connect reactants to compartments
  }

//  private def processReaction(reaction: org.sbml.jsbml.Reaction, parsedModel: SBMLModel): Unit = transaction(graphDataBaseConnection) {
////    uploader method
//    val compartment = reaction.getCompartment
//    val reactants = reaction.getListOfReactants.asScala.toList
//    val reactantObjects = reactants.map(r => new Reactant(r.getSpecies))
//    val reactionObject = new Reaction(reaction.toString, reactantObjects)
//    val compartmentObject = new Compartment(compartment)
//    val reactantSpecies = reaction.getListOfReactants.asScala.toList.map(_.getSpecies)
//
//    val species = parsedModel.getSpecies.map(_.getId)
//
//    val reactantNodes = reactantObjects.map(_.upload(graphDataBaseConnection))
//    val compartmentNode = compartmentObject.upload(graphDataBaseConnection)
//    val reactionNode = reactionObject.upload(graphDataBaseConnection)
//
//    reactantNodes.foreach(_.createRelationshipTo(compartmentNode, BiomeDBRelations.locates_in))
//    reactantNodes.foreach(_.createRelationshipTo(reactionNode, BiomeDBRelations.participates_in))
//  }
//
//
//  private def processReactant(reactant: SpeciesReference): Reactant = {
//    val reactantName = reactant
//    new Reactant(reactant.getName)
//  }
}

class SBMLModel(
                 reactions: List[org.sbml.jsbml.Reaction],
                 compartments: List[org.sbml.jsbml.Compartment],
                 definitions: List[FunctionDefinition],
                 parameters: List[Parameter],
                 rules: List[Rule],
                 species: List[Species],
                 modifiers: List[ModifierSpeciesReference]
               ) {
//  unused. To be deleted.
  def getReaction = this.reactions

  def getCompartments = this.compartments

  def getDefinitions = this.definitions

  def getParameters = this.parameters

  def getRules = this.rules

  def getSpecies = this.species

  def getModifiers = this.modifiers

}
