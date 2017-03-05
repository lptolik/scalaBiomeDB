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
  val basePath = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/"
  //  val basePath = "/home/artem/work/2017/staphylococcus/"
  val localDB = new File(basePath + "data/graph.db")
//  val localDB = new File(basePath + "neo4j-community-2.3.1/data/graph.db")
  val db = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)

  val organism = "Escherichia coli W"
//  val organism = "Staphylococcus aureus subsp. aureus N315"

  val model = JSBMLExport.assembleModel(organism, "biograph_export_model")(db)

  val out = basePath + "sbml_out/out.xml"
  JSBMLExport.writeToFile(model, out)
}
