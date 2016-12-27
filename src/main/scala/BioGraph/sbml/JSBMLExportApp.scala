package BioGraph.sbml

import java.io.File

import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.TransactionSupport

import scala.collection.JavaConverters._

/**
  * Created by piane_ramso on 12/27/16.
  */
object JSBMLExportApp extends App with TransactionSupport {
  val basePath = "/Users/piane_ramso/Ya.Disk/Yandex.Disk/Studying/PhD/thesis/pushchono_phd/"
  val localDB = new File(basePath + "data/graph.db")
  val db = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)

  val reactionsNodes = transaction(db) {
    db.findNodes(DynamicLabel.label("Reaction")).asScala.toList
  }

  val model = JSBMLExport.assembleModel(reactionsNodes, "biograph_export_model")(db)
  val out = basePath + "sbml_out/out.xml"
  JSBMLExport.writeToFile(model, out)
}
