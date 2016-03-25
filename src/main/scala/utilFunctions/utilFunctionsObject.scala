import BioGraph._
package utilFunctions {

  import java.util
  import java.util.function.Consumer

  import org.neo4j.cypher.internal.compiler.v1_9.symbols.RelationshipType
  import org.neo4j.graphdb.traversal.Evaluators
  import org.neo4j.graphdb.{RelationshipType, Relationship, Direction, Result, ResourceIterator, DynamicLabel, Transaction, Node}
  import java.security.MessageDigest
  import java.io.File
  import org.neo4j.graphdb.factory.GraphDatabaseFactory
  import utilFunctions.BiomeDBRelations
  import scala.collection.immutable.Map
  import scala.collection.mutable
  import scala.collection.parallel.immutable.ParHashMap
  import scala.io.Source
  import scala.collection.immutable.HashMap
  import scala.collection.JavaConverters
  import scala.collection.JavaConverters._

  /**
    * Created by artem on 12.02.16.
    */
  object utilFunctionsObject {
    //    def getCoordinates(properties: Map[String, Any]): List[Any] = {
    //      List(properties("start"), properties("end"), properties("strand"))
    //    }

    def getCoordinates(properties: Map[String, Any]): Coordinates = properties("coordinates") match {
      case Some(x: Coordinates) => x
      case None => throw new Exception("Feature coordinates are missing!")
      case _ => throw new Exception("Coordinates must of Coordinates type!")
      //    List(properties("start"), properties("end"), properties("strand"))
    }

    def getLengthProperty(properties: Map[String, Any]): Int = properties("length") match {
      case Some(x: Int) => x
      case None => throw new Exception("Feature length is missing!")
      case _ => throw new Exception("Length must of Int type!")
    }

    //  def getSourceProperty(properties: Map[String, Any]): List[String] = properties("source") match {
    //    case Some(x: List[String]) => x
    //    case None => throw new Exception("Node has no source!")
    //    case _  => throw new Exception("Source must be List[String] type!")
    //  }

    //  def collectSequences(organism: Organism, taxon: Taxon): List[Sequence] = {
    //    // using Java API get Sequences.
    //    // resultPolyList = filter(poly.getOrganism == organism)
    ////     writeToFile(">" + poly.getSequence.getId + "/n" + poly.getSequence.getSequence + "\n")
    //    ???
    //  }

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

    def readBlastResultFile(readFunction: (Iterator[String], Map[Int, Sequence], Source) => Map[Int, Sequence])(resultFileName: String): Map[Int, Sequence] = {
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
        val evalue: Double = line.split("\t")(10).toDouble
        val identity: Double = line.split("\t")(1).toDouble
        val currentSeq =
          if (blastedSequences contains querySeqId) blastedSequences(querySeqId)
          else new Sequence(sequence = querySeq, md5 = queryMD5, nodeId = querySeqId)
        for (targetSeqId <- line.split("\t")(3).split('|').tail) {
          val similarSeq = new Sequence(
            sequence = targetSeq,
            md5 = targetMD5,
            nodeId = targetSeqId.toInt)
          currentSeq.addSimilarity(new Similarity(similarSeq, evalue, identity))
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
                              blastedSequences: Map[Int, Sequence], source: Source): Map[Int, Sequence] = {
      if (currentLines.hasNext) {
        val line = currentLines.next()
        val splitString: Array[String] = line.split('\t')
        val querySeqId: Int = splitString(0).toInt
        val querySeq: String = splitString.last
        val targetSeq: String = splitString(2)
        val evalue: Double = splitString(10).toDouble
        val identity: Double = splitString(1).toDouble
        val currentSeq =
          if (blastedSequences contains querySeqId) blastedSequences(querySeqId)
          else new Sequence(sequence = querySeq, nodeId = querySeqId)
        val similarSeq = new Sequence(sequence = targetSeq)
        currentSeq.addSimilarity(new Similarity(similarSeq, evalue, identity))
        outsideBlastReadLoop(currentLines, blastedSequences + (querySeqId -> currentSeq), source)
      }
      else {
        source.close()
        blastedSequences
      }
    }

    def readInsideBlastResultFile(name: String): Map[Int, Sequence] = readBlastResultFile(insideBlastReadLoop)(name)

    //  def readOutsideBlastResultFile(name: String): Map[Int, Sequence] = readBlastResultFile(outsideBlastReadLoop, name)

    //  def uploadBlastResults(newSequencesList: List[Sequence]): Unit ={
    ////    using Java API upload/update nodes
    //  }

    def md5ToString(inputString: String): String = {
      MessageDigest.getInstance("MD5").digest(inputString.getBytes()).map("%02X".format(_)).mkString
    }

    def makeSequencesForPolypeptidesCypher(
                                            pathToDataBase: String,
                                            skipValue: Int,
                                            previousMD5: String,
                                            previousSequenceID: String): Tuple2[String, String] = {
      val dataBaseFile = new File(pathToDataBase)
      val gdb = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
      val transaction: Transaction = gdb.beginTx()
      val polypeptidesIterator = gdb.findNodes(DynamicLabel.label("Polypeptide"))
      try{
        val cypherQueryResult = gdb.execute(
          "match (p:Polypeptide) " +
          "where exists(p.seq) " +
          "return p.seq, ID(p) " +
          "order by p.seq " +
          "skip " + skipValue + " limit 100000")
        def loop(queryResult: Result, previousMD5: String, previousSequenceID: String): Tuple2[String, String] = {
          if (queryResult.hasNext){
            val currentPolypeptide = queryResult.next()
            val currentSequence = currentPolypeptide.get("p.seq").toString
            val currentMD5 = utilFunctionsObject.md5ToString(currentSequence)
            val currentPolypeptideID = currentPolypeptide.get("ID(p)")
            if (previousMD5 equals currentMD5) {
              gdb.execute(
                "START p=node(" + currentPolypeptideID + "), s=node(" + previousSequenceID + ") " +
                  "CREATE (p)-[:IS_A]->(s)")
              loop(queryResult, previousMD5, previousSequenceID)
            }
            else {
              val created = gdb.execute(
                "START p=node(" + currentPolypeptideID + ") " +
                  "CREATE (s:Sequence:AA_Sequence {md5: '" +  currentMD5 + "', seq: '" + currentSequence + "'}) " +
                  "CREATE (p)-[:IS_A]->(s) " +
                  "RETURN ID(s)")
              loop(queryResult, currentMD5, created.next().get("ID(s)").toString)
            }
          }
          else Tuple2(previousMD5, previousSequenceID)
        }
        loop(cypherQueryResult, previousMD5, previousSequenceID)
        transaction.success()
      }
      finally {
        transaction.close()
        gdb.shutdown()
        println("Successful disconnect.")
      }
      Tuple2(previousMD5, previousSequenceID)
    }

    def changeSimilarRelations(pathToDataBase: String): Unit = {
      val dataBaseFile = new File(pathToDataBase)
      val gdb = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
      val transaction: Transaction = gdb.beginTx()
      val polypeptides = gdb.findNodes(DynamicLabel.label("Polypeptide"))
      try {
        def polypeptideLoop(polypeptideIter: ResourceIterator[Node], size: Int): Unit = {
          if (polypeptideIter.hasNext && size < 600000) {
            val polypeptideNode = polypeptideIter.next
            val polypeptideIsARelationshipIter = polypeptideNode.getRelationships(
              Direction.OUTGOING,
              BiomeDBRelations.isA).iterator
            val polypeptideSimilarRelationshipIter = polypeptideNode.getRelationships(
              Direction.OUTGOING,
              BiomeDBRelations.similar).iterator
            if (polypeptideIsARelationshipIter.hasNext && polypeptideSimilarRelationshipIter.hasNext) {
              val findSequenceNode = checkIsASequence(polypeptideIsARelationshipIter)
              if (findSequenceNode.nonEmpty) {
                val sequenceNode = findSequenceNode.head
                def similarLoop(similarRelationshipIter: util.Iterator[Relationship], changesCounter: Int): Int = {
                  if (similarRelationshipIter.hasNext) {
                    val polypeptideSimilarRelationship = similarRelationshipIter.next
                    val similarPolypeptideNode = polypeptideSimilarRelationship.getEndNode
                    val similarPolypeptideSequenceNode = similarPolypeptideNode.getRelationships(
                      Direction.OUTGOING,
                      BiomeDBRelations.isA).iterator().next.getEndNode
                    if (
                      !checkRelationExistence(sequenceNode, similarPolypeptideSequenceNode)
                        && !(sequenceNode equals similarPolypeptideSequenceNode)) {
                      sequenceNode.createRelationshipTo(similarPolypeptideSequenceNode, BiomeDBRelations.similar)
                    }
                    polypeptideSimilarRelationship.delete()
                    similarLoop(similarRelationshipIter, changesCounter + 1)
                  }
                  else changesCounter
                }
                val changesCounter = similarLoop(polypeptideSimilarRelationshipIter, 0)
                if (size > 10000) println(size + " polypeptides updated.")
                polypeptideLoop(polypeptideIter, size + changesCounter)
              }
              else polypeptideLoop(polypeptideIter, size)
            }
            else polypeptideLoop(polypeptideIter, size)
          }
        }
        polypeptideLoop(polypeptides, 0)
        transaction.success()
      }
      finally {
        transaction.close()
        gdb.shutdown()
      }
    }

    def checkRelationExistence(nodeA: Node, nodeB: Node): Boolean = {
      def relationshipLoop(relationshipIterator: util.Iterator[Relationship]): Boolean = {
        if (relationshipIterator.hasNext) {
          if (relationshipIterator.next.getOtherNode(nodeA) equals nodeB) true
          else relationshipLoop(relationshipIterator)
        }
        else false
      }
      val nodeARelationships = nodeA.getRelationships().iterator()
      relationshipLoop(nodeARelationships)
    }

    def checkIsASequence(isARelationshipIter: util.Iterator[Relationship]): List[Node] = {
      val isARelationships = isARelationshipIter.asScala.toList
      def loop(rels: List[Relationship]): List[Node] = {
        if (rels.nonEmpty) {
          val currentNode = rels.head.getEndNode
          val nodeLabels = currentNode.getLabels.asScala.toList
          if (nodeLabels contains DynamicLabel.label("Sequence")) List(currentNode)
          else loop(rels.tail)
        }
        else List()
      }
      loop(isARelationships)
    }

    def makeSequencesForPolypeptides(pathToDataBase: String): Unit = {
      val dataBaseFile = new File(pathToDataBase)
      val gdb = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
      val transactionToRead: Transaction = gdb.beginTx()
      val polypeptidesIterator = gdb.findNodes(DynamicLabel.label("Polypeptide"))
      try {
//        val td = gdb.traversalDescription()
        val transactionToWrite: Transaction = gdb.beginTx()
        def processingPolypeptidesLoop(
                                        polypeptideIterator: ResourceIterator[Node],
                                        seqCollector: ParHashMap[String, Node],
                                        transactionSize: Int,
                                        transactionToWrite: Transaction): Unit = {
          if (polypeptideIterator.hasNext) {
            val polyNode = polypeptideIterator.next()

            def defineCurrentTransaction(): Transaction = {
              if (transactionSize % 150000 == 0) {
                transactionToWrite.success()
                transactionToWrite.close()
                println("Transaction commited, counter = " + transactionSize, seqCollector.size)
                val currentTransaction = gdb.beginTx()
                currentTransaction
              }
              else transactionToWrite
            }

            val currentTransaction = defineCurrentTransaction()
            if (polyNode.hasProperty("seq")) {
              val polySequence = polyNode.getProperty("seq").toString
              val polyMD5 = md5ToString(polySequence)
              if (seqCollector.contains(polyMD5)) {
                polyNode.createRelationshipTo(seqCollector(polyMD5), BiomeDBRelations.isA)
                processingPolypeptidesLoop(polypeptideIterator, seqCollector, transactionSize + 1, currentTransaction)
              }
              else {
                val sequenceNode = gdb.createNode(DynamicLabel.label("Sequence"), DynamicLabel.label("AA_Seqeunce"))
                sequenceNode.setProperty("md5", polyMD5)
                sequenceNode.setProperty("seq", polySequence)
                polyNode.createRelationshipTo(sequenceNode, BiomeDBRelations.isA)
                processingPolypeptidesLoop(polypeptideIterator, seqCollector + (polyMD5 -> sequenceNode), transactionSize + 1, currentTransaction)
              }
            }
            else processingPolypeptidesLoop(polypeptideIterator, seqCollector, transactionSize, currentTransaction)
          }
          else transactionToWrite.close()
        }
        processingPolypeptidesLoop(polypeptidesIterator, ParHashMap(), 1, transactionToWrite)
      }
      finally {
        transactionToRead.close()
        gdb.shutdown()
        println("Successful disconnect.")
      }
    }
  }
}