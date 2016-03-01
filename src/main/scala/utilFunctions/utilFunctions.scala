package utilFunctions

import java.security.MessageDigest

import BioGraph._
import org.neo4j.kernel.api.Neo4jTypes

import scala.Predef
import scala.io.Source

/**
  * Created by artem on 12.02.16.
  */
object utilFunctions {
//    def getCoordinates(properties: Map[String, Any]): List[Any] = {
//      List(properties("start"), properties("end"), properties("strand"))
//    }

  def getCoordinates(properties: Map[String, Any]): Coordinates = properties("coordinates") match{
    case Some(x: Coordinates) => x
    case None => throw new Exception("Feature coordinates are missing!")
    case _ => throw new Exception("Coordinates must of Coordinates type!")
//    List(properties("start"), properties("end"), properties("strand"))
  }

  def getLengthProperty(properties: Map[String, Any]): Int = properties("length") match {
    case Some(x: Int) => x
    case None => throw new Exception("Feature length is missing!")
    case _  => throw new Exception("Length must of Int type!")
  }

//  def getSourceProperty(properties: Map[String, Any]): List[String] = properties("source") match {
//    case Some(x: List[String]) => x
//    case None => throw new Exception("Node has no source!")
//    case _  => throw new Exception("Source must be List[String] type!")
//  }

  def collectSequences(organism: Organism, taxon: Taxon): List[Sequence] = {
    // using Java API get Sequences.
    // resultPolyList = filter(poly.getOrganism == organism)
//     writeToFile(">" + poly.getSequence.getId + "/n" + poly.getSequence.getSequence + "\n")
    ???
  }

//  def readInsideBlastResultFile(resultFileName: String): List[Sequence] = {
//    val source = Source.fromFile(resultFileName)
//    val lines = source.getLines()
//
//    def insideBlastReadLoop(
//              currentLines: Iterator[String],
//              blastedSequences: List[Sequence]): List[Sequence] = {
//
//      if (lines.hasNext) {
//        var matchedSequences: List[Sequence] = List()
//        val line = lines.next()
//        val queryMD5: String = line.split('\t')(0).split('|')(0)
//        val querySeqId: Int = line.split('\t')(0).split('|')(1).toInt
//        val querySeq: String = line.split('\t').last
//        val targetSeq: String = line.split("\t")(2)
//        val targetMD5: String = line.split("\t")(3).split('|')(0)
//        for (targetSeqId <- line.split("\t")(3).split('|').tail){
//          matchedSequences = new Sequence(
//            sequence = targetSeq,
//            md5 = targetMD5,
//            nodeId = targetSeqId.toInt) :: matchedSequences
//        }
//        insideBlastReadLoop(
//          lines,
//          new Sequence(
//            sequence = querySeq,
//            md5 = queryMD5,
//            similarities = matchedSequences,
//            nodeId = querySeqId) :: blastedSequences)
//      }
//      else {
//        source.close()
//        blastedSequences
//      }
//    }
////    def outsideBlastReadLoop(
////                              currentLines: Iterator[String],
////                              blastedSequences: List[Sequence]): List[Sequence] = {
////      if (lines.hasNext) {
////        var matchedSequences: List[Sequence] = List()
////        val line = lines.next()
////        val querySeqId: Int = line.split('\t')(0).toInt
////        val querySeq: String = line.split('\t').last
////        val targetSeq: String = line.split("\t")(2)
////      }
////      else {
////        source.close()
////        blastedSequences
////      }
////    }
//    insideBlastReadLoop(lines, List())
//  }

  def readBlastResultFile(readFunction:(Iterator[String], Map[Int, Sequence], Source) => Map[Int, Sequence])(resultFileName: String): Map[Int, Sequence] = {
    val source = Source.fromFile(resultFileName)
    val currentLines = source.getLines()
    readFunction(currentLines, Map(), source)
  }

  def insideBlastReadLoop(
                           currentLines: Iterator[String],
                           blastedSequences: Map[Int, Sequence], source: Source): Map[Int, Sequence] = {
    if (currentLines.hasNext) {
      val line = currentLines.next()
      val queryMD5: String = line.split('\t')(0).split('|')(0)
      val querySeqId: Int = line.split('\t')(0).split('|')(1).toInt
      val querySeq: String = line.split('\t').last
      val targetSeq: String = line.split("\t")(2)
      val targetMD5: String = line.split("\t")(3).split('|')(0)
      val currentSeq =
        if (blastedSequences contains querySeqId) blastedSequences(querySeqId)
        else new Sequence(sequence = querySeq, md5 = queryMD5, nodeId = querySeqId)
      for (targetSeqId <- line.split("\t")(3).split('|').tail) {
        val similarSeq = new Sequence(
          sequence = targetSeq,
          md5 = targetMD5,
          nodeId = targetSeqId.toInt)
        currentSeq.addSimilarity(similarSeq)
      }
      insideBlastReadLoop(currentLines, blastedSequences + (querySeqId -> currentSeq), source)
    }
    else {
      source.close()
      blastedSequences
    }
  }

  def outsideBlastReadLoop(
                            currentLines: Iterator[String],
                           blastedSequences: Map[Int, Sequence], source: Source): Map[Int, Sequence] ={
    if (currentLines.hasNext) {
      val line = currentLines.next()
      val splitString = line.split('\t')
      val querySeqId = splitString(0).toInt
      val querySeq = splitString.last
      val targetSeq = splitString(2)
      val currentSeq =
        if (blastedSequences contains querySeqId) blastedSequences(querySeqId)
        else new Sequence(sequence = querySeq, nodeId = querySeqId)
      val similarSeq = new Sequence(sequence = targetSeq)
      currentSeq.addSimilarity(similarSeq)
      outsideBlastReadLoop(currentLines, blastedSequences + (querySeqId -> currentSeq), source)
    }
    else {
      source.close()
      blastedSequences
    }
  }

  def readInsideBlastResultFile(name: String): Map[Int, Sequence] = readBlastResultFile(insideBlastReadLoop)(name)

//  def readOutsideBlastResultFile(name: String): Map[Int, Sequence] = readBlastResultFile(outsideBlastReadLoop, name)

  def uploadBlastResults(newSequencesList: List[Sequence]): Unit ={
//    using Java API upload/update nodes
  }

def md5ToString(inputString: String): String = {
  MessageDigest.getInstance("MD5").digest(inputString.getBytes()).map("%02X".format(_)).mkString
}

}
