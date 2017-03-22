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

  models.foreach(uploadOneModel)

  def uploadOneModel(smblModelFile: File): Unit = {
    val model = jsbml.jsbmlModelFromFile(smblModelFile)
    val organism = model.getName

    println(s"Uploading model of '$organism' organism from ${smblModelFile.getName} file")
    jsbml.uploadModel(organism, Set("G_s0001"))(model)
  }
}
