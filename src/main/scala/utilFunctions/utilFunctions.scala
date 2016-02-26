package utilFunctions

import java.security.MessageDigest

import BioGraph._
import org.neo4j.kernel.api.Neo4jTypes

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

  def getSourceProperty(properties: Map[String, Any]): List[String] = properties("source") match {
    case Some(x: List[String]) => x
    case None => throw new Exception("Node has no source!")
    case _  => throw new Exception("Source must be List[String] type!")
  }

  def collectSequences(organism: Organism, taxon: Taxon): List[Sequence] = {
    // using Java API get Sequences.
    // resultPolyList = filter(poly.getOrganism == organism)
//     writeToFile(">" + poly.getSequence.getId + "/n" + poly.getSequence.getSequence + "\n")
    ???
  }

  def readBlastResultFile(resultFileName: String): (List[Sequence], List[Sequence]) = {
//    somehow read the UBLAST output fasta file
//    def loop(fileLineIter, creatingSequence, existingSeq): (List[Sequence], List[Sequence]) = {
//      if (fileReadHandler.getLine.head == ">") {
//        ...
//        nextExistingSeq = new Sequence(sequenceText, id=readId)
    //    loop(fileLineIter, creatingSequence, nextExistingSeq)
//      else:
//        ...
//        newSeq = new Sequence(sequenceText, id=-1, similarities = List(existingSeq))
//        loop(fileLineIter, newSeq ::: creatingSequence, existingSeq)
//    }
//    loop(fileLineIter, List(), new Sequence("start"))
    ???
  }

  def uploadBlastResults(newSequencesList: List[Sequence]): Unit ={
//    using Java API upload/update nodes
  }

def md5ToString(inputString: String): String = {
  MessageDigest.getInstance("MD5").digest(inputString.getBytes()).map("%02X".format(_)).mkString
}

}
