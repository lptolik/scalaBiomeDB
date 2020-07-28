package utilFunctions

import java.io.{File, PrintWriter}
import java.util

import BioGraph._
import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.Direction._
import org.neo4j.graphdb.{Label, GraphDatabaseService, Node, ResourceIterator}
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import scala.collection.JavaConverters._
import scala.io.Source
import scala.sys.process.Process
import utilFunctions.TransactionSupport
/**
  * Created by artem on 27.03.16.
  */

class WorkWithGraph(pathToDataBase: String) extends TransactionSupport {
  val dataBaseFile = new File(pathToDataBase)
  val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)

  def getAllNodesByLabel(requiredLabel: String): Iterator[Node] = transaction(graphDataBaseConnection) {
    val resultNodes = graphDataBaseConnection.findNodes(Label.label(requiredLabel))
    resultNodes.asScala
  }
}


class BlastUtil(pathToDataBase: String) extends WorkWithGraph(pathToDataBase) {

  val logger = LogManager.getLogger(this.getClass.getName)
  logger.info("BlastUtil object is initialized.")

  def createMapOfSequences(sequenceNode: Node): (String, Long) = transaction(graphDataBaseConnection){
    val md5 = sequenceNode.getProperties("md5").values().asScala.head.toString
    val nodeId = sequenceNode.getId
    (md5, nodeId)
  }
  //  read ID of existing Sequence and its other parameters
  var sequenceNodeCollectorPoly = getAllAASequencesNodes.map(createMapOfSequences).toMap

  var sequenceNodeCollectorDNA = getAllDNASequencesNodes.map(createMapOfSequences).toMap


  def getAllAASequencesNodes = getAllNodesByLabel("AA_Sequence")

  def getAllDNASequencesNodes = getAllNodesByLabel("DNA_Sequence")

  def applyOperationToNodes(nodeIterator: ResourceIterator[Node])
                           (makeSomethingWithNodes: (Node) => Unit): Unit = transaction(graphDataBaseConnection){
    logger.debug("Transaction in applyOperationToNodes")
    nodeIterator.asScala.foreach(makeSomethingWithNodes)
  }

  def writeNodesInfoToFile(nodeIterator: Iterator[Node], filename: PrintWriter)
                           (nodesInfoWriter: (Node, PrintWriter) => Unit): Unit = transaction(graphDataBaseConnection){
    logger.debug("Transaction in writeNodesInfoToFile")
    nodeIterator.foreach(nodesInfoWriter(_, filename))
  }

  def makeSequencesFastaFile(
                   nodeIterator : Iterator[Node],
                   outputFastaFileName: String,
                   byMD5: Boolean = false) = {
    val outputFastaFile = new PrintWriter(outputFastaFileName)
    def writeSequencesIntoFastaFile(sequenceNode: org.neo4j.graphdb.Node, outputFastaFile: PrintWriter): Unit = {
      val sequenceId = sequenceNode.getId.toString
      val sequenceSeq = sequenceNode.getProperty("seq").toString
      val md5 = utilFunctionsObject.md5ToString(sequenceSeq)

      if (byMD5) outputFastaFile.write(">" + md5 + "\n" + sequenceSeq + "\n")
      else outputFastaFile.write(">" + sequenceId + "\n" + sequenceSeq + "\n")
    }
    writeNodesInfoToFile(nodeIterator, outputFastaFile)(writeSequencesIntoFastaFile)
    outputFastaFile.close()
  }

  def callUblast(ublastLocation: String)(ublastCommand: String): Unit = {
    val ublastResultOutput = Process(ublastLocation + ublastCommand).!!
    println(ublastResultOutput)
  }

  def makeUDB(ublastLocation: String, fastaFileLocation: String, udbFileLocation: String): Unit = {
    callUblast(
      ublastLocation)(
      " -makeudb_ublast " +
      fastaFileLocation +
      " -output " +
      udbFileLocation
    )
  }

  def makeUblast(
                  ublastLocation: String,
                  fastaInputFileLocation: String,
                  udbFileLocation: String,
                  outputTextFile: String): Unit = {
    val command =
        " -ublast " +
        fastaInputFileLocation +
        " -db " + udbFileLocation +
        " -evalue 1e-5" + " -mid 70.0" +
        " -self" +
        " -minqt 0.5" +
        " -maxaccepts 500" +
        " -userout " +
        outputTextFile +
        " -userfields query+id+tseq+target+qs+ts+qlor+qhir+tlor+thir+evalue+qseq"
    callUblast(ublastLocation)(command)
  }

  def createSimilarRelationshipsForBlast(
                                          blastOutputFilename: String,
                                          dropSize: Int,
                                          outerBlastFlag: Boolean)
                                        (byMD5: Boolean = false)
                                        (polyFlag: Boolean): Int = transaction(graphDataBaseConnection){
    if (polyFlag) logger.debug("Transaction in createSimilarRelationshipsForBlast for Polypeptides")
    else logger.debug("Transaction in createSimilarRelationshipsForBlast for Genes")

    val db = polyFlag match {
      case true => DBNode("UniProtKB/Swiss-Prot")
      case false => DBNode("RefSeq")
    }

    var sequenceNodeCollector = polyFlag match {
      case true => sequenceNodeCollectorPoly
      case false => sequenceNodeCollectorDNA
    }
    val dbNode = db.upload(graphDataBaseConnection)

    var dbXRefCollector: Map[String, Node] = {
      dbNode
        .getRelationships
        .asScala
        .map(elem => elem.getStartNode.getProperty("id").toString -> elem.getStartNode)
        .toMap
    }

    val source = Source.fromFile(blastOutputFilename)
    val fullReadFileIterator = source.getLines()
    val currentIterator = fullReadFileIterator.drop(dropSize)

    def createBlastSimilarRelationship(lineList: List[String]): Unit = {

      val querySeqNode = byMD5 match {
        case true =>
          getOrCreateSequenceNode(
            utilFunctionsObject.md5ToString(lineList.head),
            lineList.head,
            lineList(2)
          )
        case false => graphDataBaseConnection.getNodeById(lineList(1).toLong)
      }

      val targetSeqNode = outerBlastFlag match {
        case true => getOrCreateSequenceNode(lineList(4), lineList(3), lineList(2))
        case false =>
          byMD5 match {
            case true => getOrCreateSequenceNode(utilFunctionsObject.md5ToString(lineList(3)), lineList(3), lineList(2))
            case false => graphDataBaseConnection.getNodeById(lineList(4).toLong)
          }
      }
      def createSimilarRelationship(querySeqNode: Node, targetSeqNode: Node): Unit = {
        val similarRelationship = querySeqNode.createRelationshipTo(targetSeqNode, BiomeDBRelations.similar)
        similarRelationship.setProperty("evalue", lineList(5).toDouble)
        similarRelationship.setProperty("identity", lineList(6).toDouble)
        similarRelationship.setProperty("length", lineList(7))
      }
      if (!utilFunctionsObject.checkRelationExistenceWithDirection(querySeqNode, targetSeqNode)) {
        createSimilarRelationship(querySeqNode, targetSeqNode)
      }
    }

    def getOrCreateSequenceNode(md5: String, seq: String, xrefId: String): Node = {
      if (sequenceNodeCollector.contains(md5)) graphDataBaseConnection.getNodeById(sequenceNodeCollector(md5))
      else {
        def getOrCreateXRef = {
          if (dbXRefCollector.contains(xrefId)) dbXRefCollector(xrefId)
          else {
            val xref = XRef(xrefId, db)
            val xrefNode = xref.upload(graphDataBaseConnection)
            dbXRefCollector ++= Map(xrefId -> xrefNode)
            xrefNode
          }
        }
        val sequence = polyFlag match {
          case true => SequenceAA(sequence = seq, md5 = md5)
          case false => SequenceDNA(sequence = seq, md5 = md5)
        }
        val sequenceNode = sequence.upload(graphDataBaseConnection)
        sequenceNodeCollector ++= Map(md5 -> sequenceNode.getId)
        val xrefNode = getOrCreateXRef
        sequenceNode.createRelationshipTo(xrefNode, BiomeDBRelations.evidence)
        sequenceNode
      }
    }

    def createBlastRelationships(currentString: String): Unit = {
      val lineList = outerBlastFlag match {
        case true => parseStringOfOuterBlast(currentString, polyFlag)
        case false => parseStringOfInnerBlast(currentString, polyFlag)
      }
      lineList.tail.nonEmpty match {
        case true => createBlastSimilarRelationship(lineList)
        case false => logger.warn("Sequence has non-canonical amino acids: " + lineList.head)
      }
    }

    logger.debug("Transaction in 500000 nodes start")
    currentIterator.take(500000).foreach(createBlastRelationships)
    println("Number of lines: " + dropSize)
    logger.debug("Transaction in 500000 nodes finish")
//    if (dropSize < iteratorSize) createSimilarRelationshipsFromInsideBlast(blastOutputFilename, dropSize + 500000)
    dropSize
  }

  private def sequenceChecker(sequence: String, polyFlag: Boolean): Boolean = {
    polyFlag match {
      case true => utilFunctionsObject.checkSequenceAA(sequence)
      case false => utilFunctionsObject.checkSequenceDNA(sequence)
    }
  }

  private def parseStringOfInnerBlast(currentString: String, polyFlag:Boolean): List[String] = {
    val splitString = currentString.split('\t')
    val querySeqId: String = splitString(0)
    val querySeq: String = splitString.last.toUpperCase
    val targetSeq: String = splitString(2).toUpperCase
    val targetSeqId: String = splitString(3)
    val evalue: String = splitString(10)
    val identity: String = splitString(1)
    val length: String = splitString(4)
    sequenceChecker(targetSeq, polyFlag) match {
      case true =>
        List(
          querySeq,
          querySeqId,
          querySeq,
          targetSeq,
          targetSeqId,
          evalue,
          identity,
          length
        )
      case false => List(targetSeq)
    }
  }

  private def parseStringOfOuterBlast(currentString: String, polyFlag: Boolean): List[String] = {
    val splitString = currentString.split('\t')
    val querySeqId: String = splitString(0)
    val querySeq: String = splitString.last.toUpperCase
    val targetSeq: String = splitString(2).toUpperCase
    val evalue: String = splitString(10)
    val identity: String = splitString(1)
    val md5: String = utilFunctionsObject.md5ToString(targetSeq)
    val uniprotXref: String = splitString(3)
    val length: String = splitString(4)
    sequenceChecker(targetSeq, polyFlag) match {
      case true =>
        List(
          querySeq,
          querySeqId,
          uniprotXref,
          targetSeq,
          md5,
          evalue,
          identity,
          length
        )
      case false => List(targetSeq)
    }
  }

  def makeBlast(blastOutputFilename: String, dropSize: Int)(byMD5: Boolean)(outerBlastFlag: Boolean)(polyFlag: Boolean): Unit = {
    if (blastOutputFilename != "-") {
      val iteratorSize = Source.fromFile(blastOutputFilename).getLines().size
      logger.debug("Number of lines: " + iteratorSize)
      def loop(res: Int): Unit = {
        val nextRes = createSimilarRelationshipsForBlast(blastOutputFilename, res, outerBlastFlag)(byMD5)(polyFlag)
        if (nextRes < iteratorSize) loop(nextRes + 500000)
      }
      loop(0 + dropSize)
    }
  }

  def makePolyInnerBlastByMD5(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = true)(outerBlastFlag =  false)(polyFlag =  true)
  def makePolyInnerBlastByID(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = false)(outerBlastFlag =  false)(polyFlag =  true)

  def makePolyOuterBlastByMD5(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = true)(outerBlastFlag = true)(polyFlag = true)
  def makePolyOuterBlastByID(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = false)(outerBlastFlag = true)(polyFlag = true)

  def makeGeneInnerBlastByMD5(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = true)(outerBlastFlag = false)(polyFlag = false)
  def makeGeneInnerBlastByID(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = false)(outerBlastFlag = false)(polyFlag = false)

  def makeGeneOuterBlastByMD5(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = true)(outerBlastFlag = true)(polyFlag = false)
  def makeGeneOuterBlastByID(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(byMD5 = false)(outerBlastFlag = true)(polyFlag = false)

  def makeOneOrganismBlastTask(organismName: String, sequenceType: String = "Polypeptide") = transaction(graphDataBaseConnection){
    val organism = graphDataBaseConnection.findNode(Label.label("Organism"), "name", organismName)
    val polysOrGenes = organism
      .getRelationships(BiomeDBRelations.partOf, INCOMING)
      .asScala
      .map(_.getStartNode)
      .filter(_
        .getLabels
        .asScala
        .toList
        .map(_.toString)
        .contains(sequenceType)
      )

    polysOrGenes.map(_.getSingleRelationship(BiomeDBRelations.isA, OUTGOING).getEndNode).iterator//.map(_.getProperty("sequence"))
  }
}
