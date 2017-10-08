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
    val localDir = "/home/artem/work/reps/GenBank/scalaUploadTest/problem_files"
//    val localDir = "/home/artem/work/2017/staphylococcus/genomes/"
//    val localDir = "/home/artem/work/reps/GenBank/scalaUploadTest/genomes/"
//    val localDir = "/media/artem/Elements/genomes"
//    val remoteDir = "/home/jane/genbank/genbank_files_for_metacyc/240_bacateria"
    val remoteDir = "/home/jane/graph_new_release/genomes/addOneGenome/"
    val localDB = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
//    val localDB = "/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/"
    val remoteDB = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"

    val filesToDrop = 0

    val gbFiles = utilFunctions.utilFunctionsObject.getUploadFilesFromDirectory(remoteDir, "gb").drop(filesToDrop)
    val dataBaseFile = new File(remoteDB)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)

    var totalSequenceCollector: Map[String, SequenceAA] = Map()
    var totalDBCollector: Map[String, DBNode] = Map()
    var totalTermCollector: Map[String, Term] = Map()

    def makeDicts(flag: Boolean = true) = {
      flag match {
        case true =>
          totalSequenceCollector ++= getNodesDict(graphDataBaseConnection)(getSequenceProperties, "Sequence")()
          totalDBCollector ++= getNodesDict(graphDataBaseConnection)(getDBProperties, "DB")()
          totalTermCollector ++= getNodesDict(graphDataBaseConnection)(getTermProperties, "Term")()
        case false =>
      }
    }

    makeDicts(flag = false)

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

      val processReadGenBankObjects = readGenBankObjects
        .map(_.filter((el) => el.toString != "()")
        .flatMap {
          case (a, b, c, d) => List(a, b, c, d)
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

          features.next().foreach({
            case elem: Some[Node] => elem.get.upload(graphDataBaseConnection)
            case None => })
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
