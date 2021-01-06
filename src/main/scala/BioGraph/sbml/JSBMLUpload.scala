package BioGraph.sbml

import java.io.File

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.TransactionSupport
import utilFunctions.utilFunctionsObject._


/**
  * Created by artem on 14.07.16.
  */
object JSBMLUpload extends App with TransactionSupport {
  def main(configurationFilename: String): Unit = {
    val conf = utilFunctions.utilFunctionsObject.readConfigurationFile(configurationFilename)

//    val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
//    val localDir = "/home/artem/work/2016/JSBML/models/"
//    val localDir = "/home/artem/work/2017/Timofei/AGORA-1.01-Reconstructions/"
//    val localDB = new File("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/1500_organisms/data/graph.db")
//    val localDir = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/sbmls"
    val localDB = new File(conf(0)) // /var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db/
    val localDir = conf(1) // ...

//      val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db/")
//      val localDir = "/home/artem/work/2017/Timofei/AGORA_HEAD/"

//    val remoteDir = "/home/jane/graph_new_release/sbmlModels/AGORA/"
//    val remoteDB = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")

    val db = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)

    //TODO determine this ids before model uploading???
    //TODO just find gene products of reactions with 'spontaneous' in name
    val spontaneousReactionsGeneProductsIds = Set("G_s0001")
    val sourceDB = "BiGG"
//    val sourceDB = "Virtual Metabolic Human"
    val models = getUploadFilesFromDirectory(localDir, "xml")

    models.foreach(uploadOneModel)

    def uploadOneModel(smblModelFile: File): Unit = {
      val jsbml = new JSBMLUtil(db)

      val model = jsbml.jsbmlModelFromFile(smblModelFile)
      val organism = model.getName

      println(s"Uploading model of '$organism' organism from ${smblModelFile.getName} file")
      jsbml.uploader(sourceDB, model, spontaneousReactionsGeneProductsIds)
    }
    def uploadOneModelOrg(smblModelFile: File,taxonId: Int, organismName: String): Unit = {
      val jsbml = new JSBMLUtil(db)

      val model = jsbml.jsbmlModelFromFile(smblModelFile)
      val organism = model.getName

      println(s"Uploading model of '$organism' organism from ${smblModelFile.getName} file")
      jsbml.uploader(sourceDB, model, spontaneousReactionsGeneProductsIds,taxonId, organismName)
    }
  }
  main("/Users/lptolik/Documents/Projects/neo4j/penicillium/sbml_upload_config.1.txt")
}
