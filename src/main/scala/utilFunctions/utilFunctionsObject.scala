import BioGraph._
import org.neo4j.graphdb.GraphDatabaseService
package utilFunctions {

  import java.util

  import org.neo4j.graphdb.traversal.Evaluators
  import org.neo4j.graphdb.{Direction, DynamicLabel, GraphDatabaseService, Label, Node, Path, Relationship, RelationshipType, ResourceIterator, Result, Transaction}
  import java.security.MessageDigest
  import java.io.{File, PrintWriter}

  import org.neo4j.graphdb.factory.GraphDatabaseFactory

  import scala.collection.immutable.Map
  import scala.collection.parallel.immutable.ParHashMap
  import scala.io.Source
  import scala.collection.JavaConverters._
  import BioGraph._
  import org.biojava.bio.seq.db.FetchURL
  import org.biojava.nbio.core.sequence.DNASequence
  import org.biojava.nbio.core.sequence.io.{FastaReaderHelper, FastaWriter}
  import org.biojavax.bio.db.ncbi.GenbankRichSequenceDB

  import scala.util.Try

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

    def readBlastResultFile(readFunction: (Iterator[String], Map[Int, SequenceAA], Source) => Map[Int, SequenceAA])(resultFileName: String): Map[Int, SequenceAA] = {
      val source = Source.fromFile(resultFileName)
      val currentLines = source.getLines()
      readFunction(currentLines, Map(), source)
    }

    def insideBlastReadLoop(
                             currentLines: Iterator[String],
                             blastedSequences: Map[Int, SequenceAA], source: Source): Map[Int, SequenceAA] = {
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
          else SequenceAA(sequence = querySeq, md5 = queryMD5, nodeId = querySeqId)
        for (targetSeqId <- line.split('\t')(3).split('|').tail) {
          val similarSeq = SequenceAA(
            sequence = targetSeq,
            md5 = targetMD5,
            nodeId = targetSeqId.toInt)
          currentSeq.addSimilarity(Similarity(similarSeq, evalue, identity))
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
                              blastedSequences: Map[Int, SequenceAA], source: Source): Map[Int, SequenceAA] = {
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
          else SequenceAA(sequence = querySeq, nodeId = querySeqId)
        val similarSeq = SequenceAA(sequence = targetSeq)
        currentSeq.addSimilarity(Similarity(similarSeq, evalue, identity))
        outsideBlastReadLoop(currentLines, blastedSequences + (querySeqId -> currentSeq), source)
      }
      else {
        source.close()
        blastedSequences
      }
    }

    def readInsideBlastResultFile(name: String): Map[Int, SequenceAA] = readBlastResultFile(insideBlastReadLoop)(name)

    def readOutsideBlastResultFile(name: String): Map[Int, SequenceAA] = readBlastResultFile(outsideBlastReadLoop)(name)

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

    def matchRelationExistenceWithoutDirection(nodeA: Node, nodeB: Node): List[Node] = {
      def relationshipLoop(relationshipIterator: util.Iterator[Relationship]): List[Node] = {
        if (relationshipIterator.hasNext) {
          if (relationshipIterator.next.getOtherNode(nodeA) equals nodeB) {
            List(nodeA, nodeB)
          }
          else relationshipLoop(relationshipIterator)
        }
        else List()
      }
      val nodeARelationships = nodeA.getRelationships().iterator()
      relationshipLoop(nodeARelationships)
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

    def checkRelationExistenceWithDirectionAndProperties(
                                              nodeA: Node,
                                              nodeB: Node,
                                              relationLabel: RelationshipType,
                                              direction: Direction,
                                              propertyKey: String,
                                              propertyValue: Any): Boolean = {
      val listOfRelationships = nodeA.getRelationships(relationLabel, direction)
      listOfRelationships.asScala.exists(r => r.getProperty(propertyKey) == propertyValue)
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
                              typeOfFeature: String,
                              organismName: String): Unit =
      transaction(graphDataBaseConnection){
        val listOfCCP = getOrganismCCP(graphDataBaseConnection, organismName)
        if (listOfCCP.nonEmpty) {
          val strandList = List(Strand.forward, Strand.reverse)
          val sortedFeatures = strandList.map(strand => listOfCCP.map(orderFeatures(graphDataBaseConnection, _, DynamicLabel.label(typeOfFeature), strand)))

          def createNext(previousFeature: Node, nextFeaturePath: Path): Node = {
            val nextFeature = nextFeaturePath.endNode()
            previousFeature.createRelationshipTo(nextFeature, BiomeDBRelations.next)
            nextFeature
          }
          //      for each strand and for each CCP create NEXT relationships
          sortedFeatures.foreach(typeOfStrand =>
            typeOfStrand.filter(_.nonEmpty).foreach(listOfFeatures =>
              listOfFeatures.tail.foldLeft(listOfFeatures.head.endNode)((previousFeature, nextFeaturePath) =>
                createNext(previousFeature: Node, nextFeaturePath: Path))))
        }
        else println(s"Empty ccp list for ${organismName}.")
      }

    def makeOverlapRelationship(
                                 graphDataBaseConnection: GraphDatabaseService,
                                 typeOfFeature: String,
                                 organismName: String): Unit =
      transaction(graphDataBaseConnection){
        val listOfCCP = getOrganismCCP(graphDataBaseConnection, organismName)
        val sortedFeatures = listOfCCP.map(orderFeatures(graphDataBaseConnection, _, DynamicLabel.label(typeOfFeature), Strand.unknown))

        def createOverlap(shortList: List[Path], feature: Path): List[Path] = {
          val end = feature.endNode().getProperty("end").toString.toInt
          val overlapingFeatures = shortList.takeWhile(_.endNode().getProperty("start").toString.toInt <= end)
          overlapingFeatures.foreach(p => p.endNode().createRelationshipTo(feature.endNode(), BiomeDBRelations.overlap))
          if (overlapingFeatures.isEmpty) shortList.drop(1)
          else shortList.drop(overlapingFeatures.length)
        }

        sortedFeatures.foreach(fullList => fullList.foldLeft(fullList.drop(1))((shortList, nextFeatureNumber) => createOverlap(shortList: List[Path], nextFeatureNumber: Path)))
      }

    def getOrganismNames(graphDataBaseConnection: GraphDatabaseService): List[String] = transaction(graphDataBaseConnection){
      val names = graphDataBaseConnection
        .findNodes(DynamicLabel.label("Organism"))
        .asScala
        .toList
        .map(_.getProperty("name").toString)
      names
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

    def getUploadFilesFromDirectory(directory: String, format: String): List[File] = {
      val files = new java.io.File(directory)
        .listFiles()
        .filter(_.getName.endsWith("." + format))
        .toList
        .sortBy(- _.length)
      files
    }

    def getSequenceAAProperties(sequenceNode: Node): (String, SequenceAA) = {
      val md5 = sequenceNode.getProperty("md5").toString
      val seq = SequenceAA(
        sequence = sequenceNode.getProperty("seq").toString.toUpperCase,
        md5 = md5,
        nodeId = sequenceNode.getId
      )
      md5 -> seq
    }

    def getSequenceDNAProperties(sequenceNode: Node): (String, SequenceDNA) = {
      val md5 = sequenceNode.getProperty("md5").toString
      val seq = SequenceDNA(
        sequence = sequenceNode.getProperty("seq").toString.toUpperCase,
        md5 = md5,
        nodeId = sequenceNode.getId
      )
      md5 -> seq
    }

    def getDBProperties(dataBaseNode: Node): (String, DBNode) = {
      val name = dataBaseNode.getProperty("name").toString
      val db = DBNode(
        name = name,
        nodeId = dataBaseNode.getId
      )
      name -> db
    }

    def getTermProperties(termNode: Node): (String, Term) = {
      val text = termNode.getProperty("text").toString
      val term = Term(
        text = text,
        nodeId = termNode.getId
      )
      text -> term
    }

    def getReactantProperties(reactantNode: Node): (String, BiochemicalReactant) = {
      val name = reactantNode.getProperty("name").toString
      val reactant = BiochemicalReactant(
        name = name,
        nodeId = reactantNode.getId
      )
      name -> reactant
    }

    def getXRefProperties(dbNode: DBNode)(xrefNode: Node): (String, XRef) = {
      val id = xrefNode.getProperty("id").toString
      val xref = XRef(id, dbNode)
      (id, xref)
    }

    def getCompoundPropertiesByXRefs(xrefNode: Node): (String, Compound) = {
      val xrefId = xrefNode.getProperty("id").toString
      val compoundNode = xrefNode.getSingleRelationship(BiomeDBRelations.evidence, Direction.INCOMING).getStartNode
      val compound = Compound(compoundNode.getProperty("name").toString, nodeId = compoundNode.getId)
      xrefId -> compound
    }

    def makeNodesDict(nodeLabel: String, nodeProperty: String, processKey: String => String = {a: String => a})
                     (graphDataBaseConnection: GraphDatabaseService): Map[String, Compound] =
      transaction(graphDataBaseConnection) {
        val compoundNodes = graphDataBaseConnection.findNodes(DynamicLabel.label(nodeLabel)).asScala
        compoundNodes
          .map { c =>
            val name = c.getProperty(nodeProperty).toString
            processKey(name) -> Compound(name, nodeId = c.getId)
          }
          .toMap
      }

    def getNodesDict[T <: BioGraph.Node]
    (graphDataBaseConnection: GraphDatabaseService)
    (f: Node => (String, T), label: String)
    (filterFunc: Node => Boolean = _ => true): Map[String, T] = transaction(graphDataBaseConnection) {
      val nodes = graphDataBaseConnection.findNodes(DynamicLabel.label(label)).asScala//.toList
      val dict = nodes.filter(filterFunc)
      val res = dict.map{node => f(node)}.toMap
      res
    }

    def findExistingRelationship(
                                  graphDataBaseConnection: GraphDatabaseService,
                                  searchFromNode: Node,
                                  searchToNode: Node,
                                  direction: Direction,
                                  relationshipType: RelationshipType): List[(Relationship, Node)] = {
      val tryFindParticipation = searchFromNode.getRelationships(relationshipType, direction).asScala.toList
      val zipEdgeWithReaction = tryFindParticipation.zip(tryFindParticipation.map(_.getEndNode))
      val tryToFindReaction = zipEdgeWithReaction.dropWhile(z => z._2 != searchToNode)
      tryToFindReaction
    }

    def checkSequence(alphabet: String)(sequenceString: String): Boolean = {
      val canonicalAlphabet = s"[^$alphabet]".r
      val res = canonicalAlphabet.findFirstIn(sequenceString)
      res match {
        case Some(s) => false
        case None => true
      }
    }

    def checkSequenceAA(seqeunceString: String) = checkSequence("ARNDCEQGHILKMFPSTWYV")(seqeunceString)

    def checkSequenceDNA(seqeunceString: String) = checkSequence("ATGC")(seqeunceString)

    def readConfigurationFile(filename: String): Array[String] = {
      val reader = Source.fromFile(filename)
      val lines = reader
        .getLines()
        .toArray
        .filter(!_.contains("#"))
      reader.close()
      lines
    }

    def addMD5ToFastaHeaders(inputFile: String, outputFile: String): Unit = {
      //todo make rewrite
      val reader = Source.fromFile(inputFile)
      val stringsIterator = reader.getLines
      val writer = new PrintWriter(new File(outputFile))
//      val fr = FastaReaderHelper.readFastaDNASequence(new File(inputFile), true).asScala.iterator

      def writeSeq(header: String, seq: String) = {
//        val md5 = md5ToString(seq)
        val splitHeader = header.split(" ")
        val seqLen = seq.length
//        val newHeader = Array(splitHeader.head + s"md5|$md5") ++ splitHeader.tail ++ "\n"
        val newHeader = Array(splitHeader.head + s"size|$seqLen") ++ splitHeader.tail ++ "\n"
        println(newHeader.mkString(" "))
        writer.write(newHeader.mkString(" "))
        writer.write(seq + "\n")
      }

      def writingLoop(stringIterator: Iterator[String], seq: String, header: String): Unit = {
        if (stringsIterator.nonEmpty) {
          val currentString = stringsIterator.next()
          if (currentString.startsWith(">")) {
            println(currentString)
            writeSeq(header, seq)
            val nextHeader = currentString
            val nextSeq = stringsIterator.next()
            writingLoop(stringsIterator, nextSeq, nextHeader)
          }
          else {
//            val nextString = stringsIterator.next(
            writingLoop(stringsIterator, seq + currentString, header)
          }
        }
        else {
          writeSeq(header, seq)
        }
      }
      val firstHeader = stringsIterator.next()
      writingLoop(stringsIterator, "", firstHeader)
      reader.close()
      writer.close()
    }

    def findCCP(graphDatabaseConnection: GraphDatabaseService)(ccp: CCP, organism: Organism): Option[Node] = {
      val organismNode = graphDatabaseConnection.findNode(DynamicLabel.label("Organism"), "name", organism.getName)
      val ccps = organismNode
        .getRelationships(Direction.INCOMING, BiomeDBRelations.partOf)
        .asScala
        .map(_.getStartNode)
        .filter(c => c.hasLabel(DynamicLabel.label(ccp.getType.toString)) && c.getProperty("name") == ccp.getName)
        .toList
//      if (ccps.length > 0) Option(ccps.head) else None
      Try(ccps.head).toOption
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
        e.printStackTrace()
        tx.failure()
        dbOperation
    }

    finally {
      tx.close()
    }
  }
}
}

