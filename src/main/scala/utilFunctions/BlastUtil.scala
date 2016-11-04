package utilFunctions

import java.io.{File, PrintWriter}
import java.util

import org.apache.logging.log4j.LogManager
import org.neo4j.graphdb.{ResourceIterator, DynamicLabel, Node, GraphDatabaseService}
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

  def getAllNodesByLabel(requiredLabel: String): ResourceIterator[Node] = transaction(graphDataBaseConnection) {
    val resultNodes = graphDataBaseConnection.findNodes(DynamicLabel.label(requiredLabel))
    resultNodes
  }
}


class BlastUtil(pathToDataBase: String) extends WorkWithGraph(pathToDataBase) {

  val logger = LogManager.getLogger(this.getClass.getName)
  logger.info("BlastUtil object is initialized.")

  def getAllSequencesNodes = getAllNodesByLabel("Sequence")

  def applyOperationToNodes(nodeIterator: ResourceIterator[Node])
                           (makeSomethingWithNodes: (Node) => Unit): Unit = transaction(graphDataBaseConnection){
    logger.debug("Transaction in applyOperationToNodes")
    nodeIterator.asScala.foreach(makeSomethingWithNodes)
  }

  def writeNodesInfoToFile(nodeIterator: ResourceIterator[Node], filename: PrintWriter)
                           (nodesInfoWriter: (Node, PrintWriter) => Unit): Unit = transaction(graphDataBaseConnection){
    logger.debug("Transaction in writeNodesInfoToFile")
    nodeIterator.asScala.foreach(nodesInfoWriter(_, filename))
  }

  def makeSequencesFastaFile(
                   nodeIterator : ResourceIterator[Node],
                   outputFastaFileName: String) = {
    val outputFastaFile = new PrintWriter(outputFastaFileName)
    def writeSequencesIntoFastaFile(sequenceNode: org.neo4j.graphdb.Node, outputFastaFile: PrintWriter): Unit = {
      val sequenceId = sequenceNode.getId.toString
      val sequenceSeq = sequenceNode.getProperty("seq").toString
      outputFastaFile.write(">" + sequenceId + "\n" + sequenceSeq + "\n")
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

  def createSimilarRelationshipsForBlast(blastOutputFilename: String, dropSize: Int, outerBlastFlag: Boolean): Int = transaction(graphDataBaseConnection){
//    val iteratorSize = Source.fromFile(blastOutputFilename).getLines().size
    logger.debug("Transaction in createSimilarRelationshipsFromInsideBlast")

    val source = Source.fromFile(blastOutputFilename)
    val fullReadFileIterator = source.getLines()
    val currentIterator = fullReadFileIterator.drop(dropSize)


    def createMapOfSequences(sequenceNode: Node): (String, Long) = {
      val md5 = sequenceNode.getProperties("md5").toString
      val nodeId = sequenceNode.getId
      (md5, nodeId)
    }
    //  read ID of existing Sequence and its other parameters
    var sequenceNodeCollector = outerBlastFlag match {
      case true =>
        val sequenceNodes = getAllSequencesNodes.asScala
        sequenceNodes.map(createMapOfSequences).toMap
      case false => Map[String, Long]()
    }
    val uniprotNode = graphDataBaseConnection.findNode(DynamicLabel.label("DB"), "name", "UniProtKB/Swiss-Prot")


    def parseStringOfInnerBlast(currentString: String): List[String] = {
      val splitString = currentString.split('\t')
      val querySeqId: String = splitString(0)
      val querySeq: String = splitString.last
      val targetSeq: String = splitString(2)
      val targetSeqId: String = splitString(3)
      val evalue: String = splitString(10)
      val identity: String = splitString(1)
      List(
        querySeq,
        querySeqId,
        querySeq,
        targetSeq,
        targetSeqId,
        evalue,
        identity
      )
    }

    def parseStringOfOuterBlast(currentString: String): List[String] = {
      val splitString = currentString.split('\t')
      val querySeqId: String = splitString(0)
      val querySeq: String = splitString.last
      val targetSeq: String = splitString(2)
      val evalue: String = splitString(10)
      val identity: String = splitString(1)
      val md5 = utilFunctionsObject.md5ToString(targetSeq)
      val uniprotXref = splitString(3)
      List(
        querySeq,
        querySeqId,
        uniprotXref,
        targetSeq,
        md5,
        evalue,
        identity
      )
    }

    def createBlastSimilarRelationship(lineList: List[String]): Unit = {

      val querySeqNode = graphDataBaseConnection.getNodeById(lineList(1).toLong)
      val targetSeqNode = outerBlastFlag match {
        case true => getOrCreateSequenceNode(lineList(4), lineList(3), lineList(2))
        case false => graphDataBaseConnection.getNodeById(lineList(4).toLong)
      }
      def createSimilarRelationship(querySeqNode: Node, targetSeqNode: Node): Unit = {
        val similarRelationship = querySeqNode.createRelationshipTo(targetSeqNode, BiomeDBRelations.similar)
        similarRelationship.setProperty("evalue", lineList(5).toDouble)
        similarRelationship.setProperty("identity", lineList(6).toDouble)
      }
      if (!utilFunctionsObject.checkRelationExistenceWithDirection(querySeqNode, targetSeqNode)) {
        createSimilarRelationship(querySeqNode, targetSeqNode)
//        val uniprotXRef = graphDataBaseConnection.createNode(DynamicLabel.label("XRef"))
//        uniprotXRef.setProperty("id", lineList(2))
//        uniprotXRef.createRelationshipTo(uniprotNode, BiomeDBRelations.linkTo)
//        targetSeqNode.createRelationshipTo(uniprotXRef, BiomeDBRelations.evidence)
      }
    }

    def getOrCreateSequenceNode(md5: String, seq: String, xrefId: String): Node = {
      if (sequenceNodeCollector.contains(md5)) graphDataBaseConnection.getNodeById(sequenceNodeCollector(md5))
      else {
        val sequenceNode = graphDataBaseConnection.createNode(
          DynamicLabel.label("AA_Sequence"),
          DynamicLabel.label("Sequence")
        )
        sequenceNode.setProperty("md5", md5)
        sequenceNode.setProperty("seq", seq)
        sequenceNodeCollector ++= Map(md5 -> sequenceNode.getId)
        val xrefNode = graphDataBaseConnection.createNode(DynamicLabel.label("XRef"))
        xrefNode.setProperty("id", xrefId)
        sequenceNode.createRelationshipTo(xrefNode, BiomeDBRelations.evidence)
        xrefNode.createRelationshipTo(uniprotNode, BiomeDBRelations.linkTo)
        sequenceNode
      }
    }

    def createBlastRelationships(currentString: String): Unit = {
      if (outerBlastFlag) {
        val lineList = parseStringOfOuterBlast(currentString)
        createBlastSimilarRelationship(lineList)
      }
      else {
        val lineList = parseStringOfInnerBlast(currentString)
        createBlastSimilarRelationship(lineList)
      }
    }

    logger.debug("Transaction in 500000 nodes start")
    currentIterator.take(500000).foreach(createBlastRelationships)
    println("Number of lines: " + dropSize)
    logger.debug("Transaction in 500000 nodes finish")
//    if (dropSize < iteratorSize) createSimilarRelationshipsFromInsideBlast(blastOutputFilename, dropSize + 500000)
    dropSize
  }

  def makeBlast(blastOutputFilename: String, dropSize: Int)(outerBlastFlag: Boolean): Unit = {
    val iteratorSize = Source.fromFile(blastOutputFilename).getLines().size
    logger.debug("Number of lines: " + iteratorSize)
    def loop(res: Int): Unit = {
      val nextRes = createSimilarRelationshipsForBlast(blastOutputFilename, res, outerBlastFlag)
      if (nextRes < iteratorSize) loop(nextRes + 500000)
    }
    loop(0)
  }

  def makeInnerBlast(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(false)

  def makeOuterBlast(blastOutputFilename: String, dropSize: Int) = makeBlast(blastOutputFilename, dropSize)(true)

}
