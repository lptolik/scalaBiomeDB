package BioGraph

import java.io.File

import org.apache.logging.log4j.LogManager
import org.biojava.nbio.core.sequence.DNASequence
import org.neo4j.graphdb
import org.neo4j.graphdb.{DynamicLabel, GraphDatabaseService}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.{BiomeDBRelations, TransactionSupport, utilFunctionsObject}
import utilFunctions.utilFunctionsObject._

import scala.collection.JavaConverters._
import scala.io.Source
import scala.collection.immutable.Map
import scala.util.Try

/**
  * Created by artem on 15.05.16.
  */
object GenBankUploader extends App with TransactionSupport{
  def main(configurationFilename: String) {
    val logger = LogManager.getLogger(this.getClass.getName)
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

    val conf = utilFunctions.utilFunctionsObject.readConfigurationFile(configurationFilename)
    val dbDir = conf(0)
    val genomesDir = conf(2)

    val filesToDrop = 0

    val dataBaseFile = new File(dbDir)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)

    var totalSequenceAACollector: Map[String, SequenceAA] = Map()
    var totalSequenceDNACollector: Map[String, SequenceDNA] = Map()
    var totalDBCollector: Map[String, DBNode] = Map()
    var totalTermCollector: Map[String, Term] = Map()
    var totalXRefCollector: Map[String, XRef] = Map()

    def makeDicts(flag: Boolean = true) = {
      flag match {
        case true =>
          totalSequenceAACollector ++= getNodesDict(graphDataBaseConnection)(getSequenceAAProperties, "AA_Sequence")()
          totalSequenceDNACollector ++= getNodesDict(graphDataBaseConnection)(getSequenceDNAProperties, "DNA_Sequence")()
          totalDBCollector ++= getNodesDict(graphDataBaseConnection)(getDBProperties, "DB")()
          totalTermCollector ++= getNodesDict(graphDataBaseConnection)(getTermProperties, "Term")()
          totalXRefCollector ++= totalDBCollector.values.flatMap(db => getNodesDict(graphDataBaseConnection)(getXRefProperties(db), "XRef")())
        case false =>
      }
    }

    makeDicts(flag = conf(3).toBoolean)

    def makeQueueForUpload(dataListLocation: String) = {
      val fileLines = Source.fromFile(dataListLocation).getLines().toList
      val splitLines = fileLines
        .map(l => l.split('\t').tail)
        .map(e => e.head -> e.tail.head.split(','))
      splitLines

    }
    val uploadQueue = makeQueueForUpload(conf(1))

//    val gbFiles = utilFunctions.utilFunctionsObject.getUploadFilesFromDirectory(genomesDir, "gb").drop(filesToDrop)

    def uploadOneFile(taxonNode: graphdb.Node, gbFile: File): Unit = transaction(graphDataBaseConnection) {
      println(gbFile.getName)

      val gbReader = new GenBankUtil(gbFile)
      gbReader.sequenceAACollector = totalSequenceAACollector
      gbReader.sequenceDNACollector = totalSequenceDNACollector
      gbReader.externalDataBasesCollector = totalDBCollector
      gbReader.termCollector = totalTermCollector
      gbReader.xrefCollector = totalXRefCollector
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

          val check = utilFunctions.utilFunctionsObject.checkRelationExistenceWithDirection(organismNode, taxonNode)
          if (!check) organismNode.createRelationshipTo(taxonNode, BiomeDBRelations.isA)

          features.next().foreach({
            case elem: Some[Node] => elem.get.upload(graphDataBaseConnection)
            case None =>
          })
          uploader(inits.tail, features)
        }
      }
      uploader(setOfOrganismsNodes, processReadGenBankObjects)
      totalSequenceAACollector = gbReader.sequenceAACollector
      totalSequenceDNACollector = gbReader.sequenceDNACollector
      totalDBCollector = gbReader.externalDataBasesCollector
      totalTermCollector = gbReader.termCollector
      totalXRefCollector = gbReader.xrefCollector
    }

    def uploadOneTaxon(taxonAndFiles: (String, Array[String])) = transaction(graphDataBaseConnection) {
      val taxID = taxonAndFiles._1.toLong
      val taxonNode = Try(graphDataBaseConnection.findNode(DynamicLabel.label("Taxon"), "tax_id", taxID)).toOption
      taxonNode match {
        case Some(t: graphdb.Node) =>
//          val gbFiles = taxonAndFiles._2.map(n => new File(genomesDir + "genBankRecord_" + n + ".gb"))
          print(taxonAndFiles)
          val gbFiles = taxonAndFiles._2.map(n => new File(genomesDir + n + ".gb"))
          gbFiles.foreach(uploadOneFile(t, _))
        case _ =>
          val warnMessage = s"Taxon $taxID not found."
          logger.warn(warnMessage)
          print(warnMessage)
      }

    }
//    gbFiles.foreach(uploadOneFile(1, _))
    uploadQueue.foreach(uploadOneTaxon)
    graphDataBaseConnection.shutdown()

    println("Upload finished")
  }
  main("/Users/lptolik/Documents/Projects/Liverpool/Penicillium/config/penicillium_gems/genbank_upload_config.txt")
}
