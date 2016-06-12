import BioGraph._
package utilFunctions {

  import java.util
  import org.neo4j.graphdb.traversal.Evaluators
  import org.neo4j.graphdb.{Path, RelationshipType, GraphDatabaseService, Label, Relationship, Direction, Result, ResourceIterator, DynamicLabel, Transaction, Node}
  import java.security.MessageDigest
  import java.io.File
  import org.neo4j.graphdb.factory.GraphDatabaseFactory
  import scala.collection.immutable.Map
  import scala.collection.parallel.immutable.ParHashMap
  import scala.io.Source
  import scala.collection.JavaConverters._

  /**
    * Created by artem on 12.02.16.
    */
  object utilFunctionsObject extends TransactionSupport{
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
        val targetSeq: String = line.split('\t')(2)
        val targetMD5: String = line.split('\t')(3).split('|')(0)
        val evalue: Double = line.split('\t')(10).toDouble
        val identity: Double = line.split('\t')(1).toDouble
        val currentSeq =
          if (blastedSequences contains querySeqId) blastedSequences(querySeqId)
          else new Sequence(sequence = querySeq, md5 = queryMD5, nodeId = querySeqId)
        for (targetSeqId <- line.split('\t')(3).split('|').tail) {
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

    def readOutsideBlastResultFile(name: String): Map[Int, Sequence] = readBlastResultFile(outsideBlastReadLoop)(name)

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
                      !checkRelationExistenceWithoutDirection(sequenceNode, similarPolypeptideSequenceNode)
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

    def checkRelationExistenceWithoutDirection(nodeA: Node, nodeB: Node): Boolean = {
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

    def checkRelationExistenceWithDirection(nodeA: Node, nodeB: Node): Boolean = {
      val listOfRelationships = nodeA.getRelationships().asScala.toSet
      val listOfNeighbours = listOfRelationships.map(_.getEndNode)
      listOfNeighbours(nodeB)
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
    def stringToLabel(labelString: String): Label = {
      new Label {
        override def name(): String = labelString
      }
    }

    def psiExample(): Unit = {

    }

    def addRelationship(
                         graphDataBaseConnection: GraphDatabaseService,
                         fromNode: Node,
                         toNode: Node, relationship: RelationshipType): Unit = transaction(graphDataBaseConnection) {
      fromNode.createRelationshipTo(toNode, relationship)
    }

    def findNode(
                  graphDataBaseConnection: GraphDatabaseService,
                  label: String,
                  propertyKey: String,
                  propertyValue: Any
                ): Node = transaction(graphDataBaseConnection) {
      val foundNode = graphDataBaseConnection.findNode(DynamicLabel.label(label), propertyKey, propertyValue)
      foundNode
      }

    def getNode(
                  graphDataBaseConnection: GraphDatabaseService,
                  id: Long
                ): Node = transaction(graphDataBaseConnection) {
      val foundNode = graphDataBaseConnection.getNodeById(id)
      foundNode
    }

    def makeNextRelationship(
                              graphDataBaseConnection: GraphDatabaseService,
                              typeOfFeature: Label,
                              organismName: String): Unit =
      transaction(graphDataBaseConnection){
        val listOfCCP = getOrganismCCP(graphDataBaseConnection, organismName)
        if (listOfCCP.nonEmpty) {
          val strandList = List(Strand.forward, Strand.reverse)
          val sortedFeatures = strandList.map(strand => listOfCCP.map(orderFeatures(graphDataBaseConnection, _, DynamicLabel.label("Feature"), strand)))

          def createNext(previousFeature: Node, nextFeaturePath: Path): Node = {
            val nextFeature = nextFeaturePath.endNode()
            previousFeature.createRelationshipTo(nextFeature, BiomeDBRelations.next)
            nextFeature
          }
          //      for each strand and for each CCP create NEXT relationships
          sortedFeatures.foreach(typeOfStrand =>
            typeOfStrand.foreach(listOfFeatures =>
              listOfFeatures.tail.foldLeft(listOfFeatures.head.endNode)((previousFeature, nextFeaturePath) =>
                createNext(previousFeature: Node, nextFeaturePath: Path))))
        }
        else println("Empty ccp list.")
      }

    def makeOverlapRelationship(
                                 graphDataBaseConnection: GraphDatabaseService,
                                 typeOfFeature: Label,
                                 organismName: String): Unit =
      transaction(graphDataBaseConnection){
        val listOfCCP = getOrganismCCP(graphDataBaseConnection, organismName)
        val sortedFeatures = listOfCCP.map(orderFeatures(graphDataBaseConnection, _, DynamicLabel.label("Feature"), Strand.unknown))

        def createOverlap(shortList: List[Path], feature: Path): List[Path] = {
          val end = feature.endNode().getProperty("end").toString.toInt
          val overlapingFeatures = shortList.takeWhile(_.endNode().getProperty("start").toString.toInt <= end)
          overlapingFeatures.foreach(p => p.endNode().createRelationshipTo(feature.endNode(), BiomeDBRelations.overlap))
          if (overlapingFeatures.isEmpty) shortList.drop(1)
          else shortList.drop(overlapingFeatures.length)
        }

        sortedFeatures.foreach(fullList => fullList.foldLeft(fullList.drop(1))((shortList, nextFeatureNumber) => createOverlap(shortList: List[Path], nextFeatureNumber: Path)))


//        def f(full: List[Path], short: List[Path], drop: Int): Unit = {
//          short.foreach()
//        }
      }

    private def getOrganismCCP(
                                      graphDataBaseConnection: GraphDatabaseService,
                                      organismName: String): List[Long] = {
      def findCCP(p: Path): Boolean = {
        val labels = p.endNode().getLabels.asScala.toList
        labels.contains(DynamicLabel.label("Chromosome")) |
          labels.contains(DynamicLabel.label("Contig")) |
          labels.contains(DynamicLabel.label("Plasmid"))
      }

      val traversalResult = graphDataBaseConnection
        .traversalDescription()
        .breadthFirst()
        .relationships(BiomeDBRelations.partOf, Direction.INCOMING)
        .evaluator(Evaluators.toDepth(1))
        .traverse(graphDataBaseConnection.findNode(DynamicLabel.label("Organism"), "name", organismName))
      val ccpIdList = traversalResult.asScala.toList.filter(findCCP).map(_.endNode().getId)
      ccpIdList
//      val l = processRes.map(_.endNode().getId)
//      l


//      val cypherQuery = f"MATCH (org:Organism{name: '$organismName%s'})<-[:PART_OF]-(ccp) " +
//        f"WHERE ccp:Chromosome or ccp:Plasmid or ccp:Contig " +
//        f"RETURN id(ccp)"
//      val cypherResult = graphDataBaseConnection.execute(cypherQuery)
//
//      def getIds(n: String) = {
//        n.drop(1).dropRight(1).toLong
//      }
//
//      if (cypherResult.hasNext) {
//        val res = cypherResult.asScala.toList
//        res.map(el => getIds(el.values().toString))
//      }
//      else {
//        println("Nothing was found")
//        List()
//      }
    }

    private def orderFeatures(graphDataBaseConnection: GraphDatabaseService,
                              ccpID: Long,
                              typeOfFeature: Label,
                              strand: BioGraph.Strand.Value): List[Path] ={

      def sortByStartCoordinate(p1: Path, p2: Path): Boolean = {
        def getStartPosition(p: Path): Long = p.endNode().getProperty("start").toString.toInt
        getStartPosition(p1) < getStartPosition(p2)
      }
        val traversalResult = graphDataBaseConnection
          .traversalDescription()
          .breadthFirst()
          .relationships(BiomeDBRelations.partOf, Direction.INCOMING)
          .evaluator(Evaluators.toDepth(1))
          .traverse(graphDataBaseConnection.getNodeById(ccpID))
        val traversedFeatures = traversalResult.asScala.toList.drop(1)

      val sortedFeatures = traversedFeatures.sortWith(sortByStartCoordinate)

      if (strand == Strand.unknown) sortedFeatures
      else sortedFeatures.filter(_.endNode().getProperty("strand") == strand.toString)


    }

    def getGenBankFilesFromDirectory(directory: String): List[File] = {
      val files = new java.io.File(directory).listFiles().toList.sortBy(- _.length)
      val gbFiles = files.filter(_.getName.endsWith(".gb"))
      gbFiles
    }
  }

trait TransactionSupport {

  protected def transaction[A <: Any](graphDataBaseConnection: GraphDatabaseService)(dbOperation: => A): A = {
    val tx = graphDataBaseConnection.beginTx()
    try {
      val result = dbOperation
      tx.success()
      result
    }

    catch {
      case e: Exception =>
        println(e.getMessage)
        tx.failure()
        dbOperation
    }

    finally {
      tx.close()
    }
  }
}
}

