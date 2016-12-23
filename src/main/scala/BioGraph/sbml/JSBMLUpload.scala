package BioGraph.sbml

import java.io.File

import utilFunctions.TransactionSupport

/**
  * Created by artem on 14.07.16.
  */
 object JSBMLUpload extends App with TransactionSupport {
//object JSBMLUpload {
  def main(): Unit = {

    val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
//    val localDB = new File("/Users/piane_ramso/Ya.Disk/Yandex.Disk/Studying/PhD/thesis/pushchono_phd/data/graph.db")
    val localDir = "/home/artem/work/2016/JSBML/models/"
//    val localDir = "/Users/piane_ramso/Ya.Disk/Yandex.Disk/Studying/PhD/thesis/pushchono_phd/sbmls"

//    val remoteDir = "/home/jane/graph_new_release/sbmlModels"
//    val remoteDB = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")

    val models = utilFunctions.utilFunctionsObject.getUploadFilesFromDirectory(localDir, "xml")

    val jsbml = new JSBMLUtil(localDB)

    def uploadOneModel(smblModel: File): Unit = {
      println(smblModel.getName)
      val model = jsbml.processSBMLFile(smblModel)
      jsbml.uploadModels(model)
    }
    models.foreach(uploadOneModel)
  }
  main()
}
