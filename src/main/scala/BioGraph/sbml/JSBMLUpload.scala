package BioGraph.sbml

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.GZIPInputStream

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.sbml.jsbml.Model
import utilFunctions.TransactionSupport
import utilFunctions.utilFunctionsObject._

import scala.io.Source


/**
  * Created by artem on 14.07.16.
  */
object JSBMLUpload extends App with TransactionSupport {
//  val conf = utilFunctions.utilFunctionsObject.readConfigurationFile(args(0))

//      val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
  //    val localDir = "/home/artem/work/2016/JSBML/models/"
  //    val localDir = "/home/artem/work/2017/Timofei/AGORA-1.01-Reconstructions/"
//      val localDB = new File("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/data/graph.db")
  val localDB = new File("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/1500_organisms/data/graph.db")
//  val localDir = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/sbmls"
  val localDir = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/embl_gems/models/e/eggerthia"
//  val localDB = new File(conf(0)) // /var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db/
//  val localDir = conf(1) // ...

  //      val localDB = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db/")
  //      val localDir = "/home/artem/work/2017/Timofei/AGORA_HEAD/"

  //    val remoteDir = "/home/jane/graph_new_release/sbmlModels/AGORA/"
  //    val remoteDB = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")

  val db = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)
  val jsbml = new JSBMLUtil(db)

  val basePath = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/embl_gems/"
  val taxIdWithModels = Source
    .fromFile("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/embl_gems/biomedb_embl_gems_tax_ids_intersection")
    .getLines()
    .toSeq
    .map { l =>
      val fileNameIn = basePath + l.split(" ")(1)
      println(s"Processing file $fileNameIn...")

      val gzis: GZIPInputStream = new GZIPInputStream(new FileInputStream(fileNameIn))

      val fileNameOut = fileNameIn.replace(".gz", "")
      if (!new File(fileNameOut).exists()) {
        val buffer = new Array[Byte](1024)
        val out: FileOutputStream = new FileOutputStream(fileNameOut)

        var len = 1
        while (len > 0) {
          len = gzis.read(buffer)
          if (len > 0)
            out.write(buffer, 0, len)
        }

        gzis.close()
        out.close()

        println(s"File $fileNameIn ungipped and saved as $fileNameOut")
      }

      val model = jsbml.jsbmlModelFromFile(new File(fileNameOut))
      (l.split(" ")(0).toInt, model)
    }


  //TODO determine this ids before model uploading???
  //TODO just find gene products of reactions with 'spontaneous' in name
//  val spontaneousReactionsGeneProductsIds = Set("G_s0001")
  val spontaneousReactionsGeneProductsIds = Set("spontaneous")
//  val sourceDB = "BiGG"
//  val sourceDB = "Virtual Metabolic Human"
  val sourceDB = "EMBL GEMs"

  taxIdWithModels.foreach { case (taxId, model) => uploadOneModel(taxId, model) }

  def uploadOneModel(taxonId: Int, sbmlModel: Model): Unit = {

    val modelName = sbmlModel.getName
    val modelId = sbmlModel.getId

    println(s"Uploading model with name '$modelName' and id '$modelId' from ${sbmlModel.getName}")
    jsbml.uploader(sourceDB, sbmlModel, spontaneousReactionsGeneProductsIds, Some(taxonId))
  }
}
