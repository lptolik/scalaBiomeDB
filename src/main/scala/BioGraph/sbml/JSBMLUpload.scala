package BioGraph.sbml

import java.io.File

import org.neo4j.graphdb.Node
import org.sbml.jsbml.Model
import utilFunctions.TransactionSupport
import utilFunctions.utilFunctionsObject._

/**
  * Created by artem on 14.07.16.
  */
object JSBMLUpload extends App with TransactionSupport {
//    val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
  val localDB = new File("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/data/graph.db")
//    val localDir = "/home/artem/work/2016/JSBML/models/"
  val localDir = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/sbmls"

//    val remoteDir = "/home/jane/graph_new_release/sbmlModels"
//    val remoteDB = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")

  val models = getUploadFilesFromDirectory(localDir, "xml")

  val jsbml = new JSBMLUtil(localDB)

  val organism = "Escherichia coli W"
  val uploadModelOfOrganism: Model => List[Node] = jsbml.uploadModel(organism)
  models.foreach(uploadOneModel)

  //TODO 1. check if all entities are loaded correctly
  //TODO   1. get reactions, species, flat proteins list by organism from the DB
  //TODO   2. get all the same entities from JSBML
  //TODO   3. check if (1) contains all from (2)

  def uploadOneModel(smblModel: File): Unit = {
    println(smblModel.getName)
    val model = jsbml.jsbmlModelFromFile(smblModel)
    uploadModelOfOrganism(model)
  }
}
