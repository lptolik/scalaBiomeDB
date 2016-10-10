package BioGraph

import java.io.File

import org.biojava.nbio.core.sequence.DNASequence
import org.neo4j.graphdb
import org.neo4j.graphdb.{DynamicLabel, GraphDatabaseService}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.{TransactionSupport, BiomeDBRelations}
import utilFunctions.utilFunctionsObject._
import scala.collection.JavaConverters._

import scala.collection.immutable.Map

/**
  * Created by artem on 15.05.16.
  */
object GenBankUploader extends App with TransactionSupport{
  def main() {

    println("Upload started")
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/Chlamydia_trachomatis_A2497_complete_genome_ver1.gb")
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/Aliivibrio_salmonicida_LFI1238_chromosome_1.gb")
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/Aliivibrio_salmonicida_LFI1238_chromosome_2.gb")
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/ADUM01000033.gb")
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/ADUM01000034.gb")
//    val localDir = "/home/artem/work/reps/GenBank/scalaUploadTest/"
//    val localDir = "/home/artem/work/reps/GenBank/biome_api/biome/load/genbank/genbank_files_for_metacyc/"
    val localDir = "/home/artem/work/reps/GenBank/scalaUploadTest/problem_files/"
//    val localDir = "/home/artem/work/reps/GenBank/scalaUploadTest/genomes/"
//    val localDir = "/media/artem/Elements/genomes"
//    val remoteDir = "/home/jane/genbank/genbank_files_for_metacyc/240_bacateria"
    val remoteDir = "/home/jane/graph_new_release/genomes"
    val localDB = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
    val remoteDB = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"

    val filesToDrop = 0//1776//1425

//    val db = args.nonEmpty match {
//    case true => args(0)
//    case _ => remoteDB
//    }
//
//    val dir = args.nonEmpty match {
//      case true => args(1)
//      case _ => remoteDir
//    }

    val gbFiles = utilFunctions.utilFunctionsObject.getUploadFilesFromDirectory(localDir, "gb").drop(filesToDrop)
    val dataBaseFile = new File(localDB)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)


//    def getSequenceDict(graphDatabaseConnection: GraphDatabaseService): Map[String, Sequence] = transaction(graphDataBaseConnection) {
//
//      def getSequenceProperties(sequenceNode: graphdb.Node): (String, Sequence) = {
//        val md5 = sequenceNode.getProperty("md5").toString
//        val seq = new Sequence(
//          sequence = sequenceNode.getProperty("Sequence").toString,
//          md5 = md5,
//          nodeId = sequenceNode.getId
//        )
//        md5 -> seq
//      }
//
//      val sequenceNodes = graphDataBaseConnection.findNodes(DynamicLabel.label("Sequence")).asScala.toList
//      val sequenceDict = sequenceNodes.map{node => getSequenceProperties(node)}.toMap
//      sequenceDict
//    }

    def getSequenceProperties(sequenceNode: graphdb.Node): (String, Sequence) = {
      val md5 = sequenceNode.getProperty("md5").toString
      val seq = Sequence(
        sequence = sequenceNode.getProperty("seq").toString,
        md5 = md5,
        nodeId = sequenceNode.getId
      )
      md5 -> seq
    }

    def getDBProperties(dataBaseNode: graphdb.Node): (String, DBNode) = {
      val name = dataBaseNode.getProperty("name").toString
      val db = DBNode(
        name = name,
        nodeId = dataBaseNode.getId
      )
      name -> db
    }

    def getTermProperties(dataBaseNode: graphdb.Node): (String, Term) = {
      val text = dataBaseNode.getProperty("text").toString
      val term = Term(
        text = text,
        nodeId = dataBaseNode.getId
      )
      text -> term
    }

    def getNodesDict[T <: Node]
    (graphDatabaseConnection: GraphDatabaseService)
    (f: graphdb.Node => (String, T), label: String): Map[String, T] = transaction(graphDataBaseConnection) {
      val sequenceNodes = graphDataBaseConnection.findNodes(DynamicLabel.label(label)).asScala.toList
      val sequenceDict = sequenceNodes.map{node => f(node)}.toMap
      sequenceDict
    }

    //    var totalSequenceCollector: Map[String, Sequence] = Map()
    var totalSequenceCollector: Map[String, Sequence] = getNodesDict(graphDataBaseConnection)(getSequenceProperties, "Sequence")
    var totalDBCollector: Map[String, DBNode] = getNodesDict(graphDataBaseConnection)(getDBProperties, "DB")
    var totalTermCollector: Map[String, Term] = getNodesDict(graphDataBaseConnection)(getTermProperties, "Term")

    def uploadOneFile(gbFile: File): Unit = transaction(graphDataBaseConnection) {
      println(gbFile.getName)
      val gbReader = new GenBankUtil(gbFile)
      gbReader.sequenceCollector = totalSequenceCollector
      gbReader.externalDataBasesCollector = totalDBCollector
      gbReader.termCollector = totalTermCollector
      val accessions = gbReader.getAccessionsFromGenBankFile
      val setOfFeatures = accessions.values.map(gbReader.getFeatures).iterator
      val setOfOrganismsNodes = accessions.values.map(gbReader.getInitialData)
      val setOfInitialOrganismNodes = setOfOrganismsNodes.iterator
      val zipFeaturesAndInitialOrganismData = setOfInitialOrganismNodes zip setOfFeatures

      val readGenBankObjects = zipFeaturesAndInitialOrganismData.map(pair => gbReader.processFeatures(pair._1)(pair._2))

      val processReadGenBankObjects = readGenBankObjects.map(_.filter((el) => el.toString != "()").flatMap {
        case (a, b, c) => List(a, b, c)
        case (a, b) => List(a, b)
        case (a) => List(a)
      })


      def uploader(inits: Iterable[(Organism, Node with CCP, DNASequence)], features: Iterator[List[Any]]): Unit = {
        if (inits.nonEmpty) {
          val nextOrgAndCCP = inits.head
          val organismNode = nextOrgAndCCP._1.upload(graphDataBaseConnection)
          nextOrgAndCCP._1.setId(organismNode.getId)
          val ccpNode = nextOrgAndCCP._2.upload(graphDataBaseConnection)
          nextOrgAndCCP._2.setId(ccpNode.getId)

          features.next().foreach({ case elem: Node => elem.upload(graphDataBaseConnection) })
          uploader(inits.tail, features)
        }
      }
      uploader(setOfOrganismsNodes, processReadGenBankObjects)
      totalSequenceCollector = gbReader.sequenceCollector
      totalDBCollector = gbReader.externalDataBasesCollector
      totalTermCollector = gbReader.termCollector
    }
    gbFiles.foreach(uploadOneFile)
    graphDataBaseConnection.shutdown()
//    gbFiles.foreach(uploadOneFile)
    println("Upload finished")
  }
  main()
}
