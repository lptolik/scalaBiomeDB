package BioGraph

import java.io.File

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml.{Model, SpeciesReference, SBMLReader}
import utilFunctions.TransactionSupport
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
    val parsedModel = readResult.getModel
    val compartments = parsedModel.getListOfCompartments.asScala.toList
    compartmentNodes = compartments.map(elem => elem.getId -> new Compartment(elem.getName)).toMap
    parsedModel
  }

  def uploadModels(parsedModel: Model) = transaction(graphDataBaseConnection) {
    val reactions = parsedModel.getListOfReactions.asScala.toList

    def makeReactantObject(reactant: SpeciesReference): Reactant = {
      val specie = parsedModel.getSpecies(reactant.getSpecies)
      val name = specie.getName
      val compartment = specie.getCompartment
      val stoi = reactant.getStoichiometry
      new Reactant(
        name = name,
        stoichiometry = Some(stoi),
        compartment = Some(compartmentNodes(specie.getCompartment))
      )
    }

    def makeReactionObject(reaction: org.sbml.jsbml.Reaction): Reaction = {
      val reactants = reaction.getListOfReactants.asScala.toList
      val reactantObjects = reactants.map(makeReactantObject)
      reactantObjects.foreach(_.upload(graphDataBaseConnection))
      new Reaction(reaction.getName, reactantObjects)
    }

    val reactionObjects = reactions.map(makeReactionObject)
    reactionObjects.map(_.upload(graphDataBaseConnection))
  }

}

