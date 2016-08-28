package BioGraph

import java.io.File

import org.biojava.nbio.core.sequence.DNASequence
import org.neo4j.graphdb
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.{TransactionSupport, BiomeDBRelations}
import utilFunctions.utilFunctionsObject._

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
//    val remoteDir = "/home/jane/graph_new_release/genomes/"
    val remoteDir = "/home/jane/genbank/genbank_files_for_metacyc/240_bacateria"
    val localDB = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
    val remoteDB = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"

//    val db = args.nonEmpty match {
//    case true => args(0)
//    case _ => remoteDB
//    }
//
//    val dir = args.nonEmpty match {
//      case true => args(1)
//      case _ => remoteDir
//    }

    val gbFiles = utilFunctions.utilFunctionsObject.getGenBankFilesFromDirectory(remoteDir)
    val dataBaseFile = new File(remoteDB)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/e_coli_k_12.gb")
    def uploadOneFile(gbFile: File): Unit = transaction(graphDataBaseConnection) {
      println(gbFile.getName)
      val gbReader = new GenBankUtil(gbFile)
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
    }
    gbFiles.foreach(uploadOneFile)
    println("Upload finished")
  }
  main()
}
