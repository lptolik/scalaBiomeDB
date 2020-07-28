package BioGraph

import java.io.File
import org.neo4j.graphdb.Node
import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.Label
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.{BiomeDBRelations, TransactionSupport}
import scala.collection.JavaConverters._

import scala.io.Source

/**
  * Created by artem on 09.06.16.
  */
class SeedUtil(seedFile: File, dataBaseFile: File) extends TransactionSupport {

  val logger = LogManager.getLogger(this.getClass.getName)
  logger.info("SeedUtil class")
  logger.info("Start processing " + seedFile.getName)

  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
  val fileReader = Source.fromFile(seedFile.getAbsolutePath)
  val linesOfSeedFile = fileReader.getLines().toList.drop(1)
  fileReader.close()
  val accession = linesOfSeedFile.head.split("\t")(0)

  var geneFunctionDictionary = getFunctionsNodesFromDB

  var foundGenesCounter = 0

  def createSeedXrefs() = transaction(graphDataBaseConnection) {

    val seedDBNode = DBNode("SEED").upload(graphDataBaseConnection)
    val organismNode = graphDataBaseConnection.findNode(
      Label.label("Organism"),
      "accession",
      accession
    )

    def processOneSeedLine(lineInSeedFile: String): Unit = {
      val lineRecords = lineInSeedFile.split("\t")
      val geneCoordinates = Coordinates(lineRecords(3).toInt, lineRecords(4).toInt, getStrand(lineRecords(6)))
      val query = "START org=node(" + organismNode.getId + ") " +
        "MATCH (org)<-[:PART_OF]-(g:Gene{start:" + geneCoordinates.start +
        ", end:" + geneCoordinates.end +
        ", strand:'" + geneCoordinates.getStrand + "'}) " +
        "RETURN ID(g)"
      val geneNode = graphDataBaseConnection.execute(query).asScala.toList


      def createSeedXref(geneNode: org.neo4j.graphdb.Node): Unit = {
        val xrefNode = graphDataBaseConnection.createNode(Label.label("XRef"))
        xrefNode.setProperty("id", lineRecords(8).split(";Name=")(0).split("=")(1))

        geneNode.createRelationshipTo(xrefNode, BiomeDBRelations.evidence)
        xrefNode.createRelationshipTo(seedDBNode, BiomeDBRelations.linkTo)

        val functionLineSplit = lineRecords(8).split(";Name=")
        def tryToCreateFunctionForGene(split: Array[String]): Unit = split match {
          case Array(_, func) =>
            val testingNode = getOrCreateGeneFunctionNode(func)
            geneNode.createRelationshipTo(testingNode, BiomeDBRelations.isA)
          case Array(_) =>
        }
//        val func = lineRecords(8).split(";Name=")(1)
        tryToCreateFunctionForGene(functionLineSplit)
      }

      def getOrCreateGeneFunctionNode(func: String): org.neo4j.graphdb.Node = {
        if (geneFunctionDictionary.contains(func)) geneFunctionDictionary(func)
        else {
          val testingNode = graphDataBaseConnection.createNode(Label.label("Function"))
          testingNode.setProperty("function", func)
          geneFunctionDictionary += (func -> testingNode)
          testingNode
        }
      }

//      check how many gene are found
      if (geneNode.length == 1) {
        val geneNodeID = geneNode.head.get("ID(g)").toString.toLong
        createSeedXref(graphDataBaseConnection.getNodeById(geneNodeID))
        foundGenesCounter += 1
      }
      else if (geneNode.isEmpty) logger.error("Gene not found with coordinates: " + geneCoordinates)
      else logger.error("Multiple genes: " + geneCoordinates)
    }

//    read and process each line of the SEED
    linesOfSeedFile.foreach(processOneSeedLine)
  }

  private def getFunctionsNodesFromDB: Map[String, org.neo4j.graphdb.Node] = transaction(graphDataBaseConnection) {
    val functionNodes = graphDataBaseConnection.findNodes(Label.label("Function"))
    val dict = functionNodes.asScala.map{elem => elem.getProperty("function").toString -> elem}
    dict.toMap
  }

  private def getStrand(record: String): Strand.Value = record match {
    case "+" => Strand.forward
    case "-" => Strand.reverse
    case _ =>
      logger.error("Unknown strand: " + record)
      Strand.unknown
  }

}
