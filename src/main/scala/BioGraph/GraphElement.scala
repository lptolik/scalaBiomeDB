import BioGraph.{DBNode, Node, XRef, SequenceAA, Rel, BioEntity}
package BioGraph {

  import org.neo4j.graphdb
  import org.neo4j.graphdb.{Node, _}
  import utilFunctions._
  import org.neo4j.graphdb.Direction._
  import utilFunctions.BiomeDBRelations._

  import scala.util.{Failure, Success, Try}
  import scala.collection.JavaConverters._
  import scala.collection.immutable.HashSet

  /**
  * created by artem on 11.02.16.
  */


  trait GraphElement {

    def getProperties: Map[String, Any]

    def isNode: Boolean

    def isRel: Boolean

    def getId: Long

    //  type Strand = String

  }

  abstract class Node(
                       properties: Map[String, Any],
                       var id: Long = -1)
    extends GraphElement {

    def isNode = true

    def isRel = false

    def getProperties = properties

    def setProperties(newProperties: Map[String, Any]): Map[String, Any] = properties ++ newProperties

    def getLabels: List[String]

    override def toString: String = getLabels.toString()

    def getId = id

    def setId(newId: Long): Unit = id = newId

    def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      if (this.getId > -1) graphDataBaseConnection.getNodeById(this.getId)
      else {
        val graphDBNode = graphDataBaseConnection.createNode
        //    convert string to labels and add them to the node
        getLabels.map(utilFunctionsObject.stringToLabel).foreach(graphDBNode.addLabel)
        //    upload properties from the Map "properties"
        //    this.getProperties.foreach{case (k, v) => graphDBNode.setProperty(k, v)}
        this.setId(graphDBNode.getId)
        graphDBNode
      }
    }
  }

  abstract class Rel(
                      id: Long = -1,
                      start: Node,
                      end: Node,
                      properties: Map[String, Any] = Map())
    extends GraphElement {

    def isNode = false

    def isRel = true

    def setProperties(newProperties: Map[String, Any]): Unit = properties ++ newProperties

    def getProperties = properties

    def startNode = start

    def endNode = end

    override def toString = start.toString + "-[:" + getLabel + "]->" + end.toString

    def getLabel: String

    def getId = id
  }

  trait BioEntity {

  //  def addCCPNode: Unit
  //
  //  def addOrganismNode: Unit

    def getName: String
  }

  trait DNA {

    //  def getCoordinates: Coordinates

  }


  trait GeneProduct{

    def getGene: Gene

  }

  trait CCP extends Node with BioEntity {

    def getLength: Int

    def getType: CCPType.Value

    def getSource: List[String]

    def getChromType: DNAType.Value

    def getOrganism: Organism

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {

      val tryToFindNode = graphDataBaseConnection.findNode(DynamicLabel.label(getType.toString), "name", this.getName)
      if (tryToFindNode == null) {
        val newProperties = this.setProperties(Map(
          "length" -> this.getLength,
          "type" -> this.getChromType.toString,
          "name" -> this.getName,
          "source" -> this.getSource.mkString(", ")))
        val ccpNode = super.upload(graphDataBaseConnection)
        newProperties.foreach { case (k, v) => ccpNode.setProperty(k, v) }
        val organismNode = graphDataBaseConnection.getNodeById(this.getOrganism.getId)
        ccpNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
        ccpNode
      }
      else tryToFindNode

    }
  }

  //  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
  //    val tryToFindNode = graphDataBaseConnection.findNode(DynamicLabel.label("DB"), "name", this.getName)
  //    if (tryToFindNode == null) {
  //      val createdDbNode = super.upload(graphDataBaseConnection)
  //      this.setProperties(Map("name" -> this.getName)).foreach{case (k, v) => createdDbNode.setProperty(k, v)}
  //      createdDbNode
  //    }
  //    else tryToFindNode
  //}

  trait FunctionalRegion {}

  object DNAType extends Enumeration {

    type ChromType = Value

    val circular, linear, unknown = Value

    override def toString = Value.toString
  }

  object CCPType extends Enumeration {

    type CCPType = Value

    val Chromosome, Contig, Plasmid = Value

    override def toString = Value.toString
  }

  object Strand extends Enumeration {

    val forward, reverse, unknown = Value

    override def toString = Value.toString
  }

  object ReferenceSource extends Enumeration {

    type SourceType = Value

    val GenBank, MetaCyc, unknown = Value

    override def toString = Value.toString
  }

  object RelationshipDirection extends Enumeration {

    type SourceType = Value

    val to, from = Value

    override def toString = Value.toString
  }

  object TaxonType extends Enumeration {

    val genus,
    species,
    no_rank,
    family,
    phylum,
    order,
    subspecies_class,
    subgenus,
    superphylum,
    species_group,
    subphylum, suborder,
    subclass,
    varietas,
    forma,
    species_subgroup,
    superkingdom,
    subfamily,
    tribe
    = Value

    override def toString = Value.toString
  }

  case class Coordinates(
                          start: Int,
                          end: Int,
                          strand: Strand.Value) {

    require (start <= end, "Start coordinate cannot have bigger value than end coordinate!")

    def getStart = start

    def getEnd = end

    def getStrand = strand

    override def equals(that: Any): Boolean = that match {
      case that: Coordinates =>
        (that canEqual this) &&
        this.getStrand == that.getStrand &&
        this.getStart == that.getStart &&
        this.getEnd == that.getEnd
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Coordinates]

    override def hashCode: Int = 41 * (41 * (41 + start.hashCode) + end.hashCode) + strand.hashCode

    def comesBefore(that: Coordinates) = that match{
      case that: Coordinates =>
        this.getStrand == that.getStrand &&
        this.getStart < that.getStart &&
        (that canEqual this)
      case _ => false
    }

    override def toString = "(" + this.getStart.toString + ", " + this.getEnd.toString + ", " + this.getStrand.toString + ")"
  }

  case class Boundaries(
                         firstGene: Gene,
                         lastGene: Gene) {
    require(firstGene.getOrganism equals lastGene.getOrganism,
      "Genes in the operon must be located in the same organism!")

    require(firstGene.getCCP equals lastGene.getCCP,
      "Genes must be located on the same CCP!")

    require(firstGene.getCoordinates.getStrand equals lastGene.getCoordinates.getStrand,
      "Genes in the operon must be located on the same strand!")

    require(firstGene.getCoordinates comesBefore lastGene.getCoordinates,
      "Start gene coordinate cannot have bigger value than end gene coordinate!")

    def getFirstGene = firstGene

    def getLastGene = lastGene

    def getStrand = getFirstGene.getCoordinates.getStrand

    override def equals(that: Any): Boolean = that match {
      case that: Boundaries =>
        (that canEqual this) &&
        (this.getFirstGene equals that.getFirstGene) &&
        (this.getLastGene equals that.getLastGene)
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Boundaries]

    override def hashCode: Int = 41 * (41 + firstGene.hashCode) + lastGene.hashCode
  }

  case class Similarity(
                         sequence: Sequence,
                         evalue: Double,
                         identity: Double) {
    require(identity <= 100.0,
    "Identity cannot be more than 100%.")

    def getSequence = sequence

    def getEvalue = evalue

    def getIdentity = identity

  }

  case class ModelNode(sourceDBId: String,
                       sourceDBName: String,
                       properties: Map[String, String] = Map(),
                       nodeId: Long = -1) extends Node(properties, nodeId) {
    override def getLabels: List[String] = List("Model")

    override def upload(db: GraphDatabaseService): graphdb.Node = {
      if (this.getId < 0) {
        val modelNode = super.upload(db)
        modelNode.setProperty("sourceDBId", sourceDBId)
        val sourceDB = DBNode(sourceDBName)
        val xRef = XRef(sourceDBId, sourceDB).upload(db)
        modelNode.createRelationshipTo(xRef, BiomeDBRelations.evidence)
        modelNode
      }
      else {
        db.getNodeById(this.getId)
      }
    }
  }

  case class DBNode(
                     name: String,
                     properties: Map[String, String] = Map(),
                     nodeId: Long = -1)
    extends Node(properties, nodeId) {

    def getLabels = List("DB")

    def getName = name

    override def equals(that: Any) =  that match {
      case that: DBNode =>
        (that canEqual this) &&
        this.getName == that.getName
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[DBNode]

    override def hashCode = 41 * name.hashCode


    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      if (this.getId < 0) {
        val tryToFindNode = Option(graphDataBaseConnection.findNode(DynamicLabel.label("DB"), "name", this.getName))
        val dbNode = tryToFindNode match {
          case Some(n) => n
          case None =>
            val createdDBNode = super.upload(graphDataBaseConnection)
            this.setProperties(Map("name" -> this.getName)).foreach{case (k, v) => createdDBNode.setProperty(k, v)}
            createdDBNode
        }
        dbNode
      }
      else graphDataBaseConnection.getNodeById(this.getId)
    }
  }

  case class XRef(xrefId: String,
                  dbNode: DBNode,
                  properties: Map[String, String] = Map(),
                  nodeId: Long = -1)
    extends Node(properties, nodeId) {

    def getLabels = List("XRef")

    def getXRef = xrefId

    def getDB = dbNode

    override def equals(that: Any) =  that match {
      case that: XRef =>
        (that canEqual this) &&
        xrefId.toUpperCase == that.getXRef.toUpperCase
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[XRef]

    override def hashCode = 41 * xrefId.toUpperCase.hashCode

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      val newProperties = this.setProperties(Map("id" -> this.getXRef))
      val xrefNode = super.upload(graphDataBaseConnection)
      newProperties.foreach{case (k, v) => xrefNode.setProperty(k, v)}

      val dbNode = this.getDB.upload(graphDataBaseConnection)
      xrefNode.createRelationshipTo(dbNode, BiomeDBRelations.linkTo)

      xrefNode
    }
  }

  abstract class Feature(coordinates: Coordinates,
                         properties: Map[String, Any] = Map(),
                         ccp: CCP,
                         source: List[String],
                         nodeId: Long = -1)
    extends Node(properties, nodeId) {

    def getCoordinates = coordinates

    def getLabels = List("Feature", "DNA")

    def next = throw new Exception("Not implemented yet!")

    def previous = throw new Exception("Not implemented yet!")

    def overlaps = throw new Exception("Not implemented yet!")

    def getCCP = ccp

    def getSource = source

    override def equals(that: Any) = that match {
      case that: Feature => this.getCCP == that.getCCP &&
        this.getCoordinates == that.getCoordinates
      case _ => false
    }

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      val newProperties = this.setProperties(
        Map(
          "start" -> this.getCoordinates.start,
          "end" -> this.getCoordinates.end,
          "strand" -> this.getCoordinates.strand.toString,
          "source" -> this.getSource.mkString(", ")))
      val featureNode = super.upload(graphDataBaseConnection)
      newProperties.foreach{case (k, v) => featureNode.setProperty(k, v)}
      featureNode
    }

  //  def canEqual(that: Any): Boolean
  }

  case class Gene(
                   name: String,
                   coordinates: Coordinates,
                   ccp: CCP,
                   terms: List[Term],
                   organism: Organism,
                   source: List[String],
                   sequence: SequenceDNA,
                   properties: Map[String, Any] = Map(),
                   nodeId: Long = -1)
    extends Feature(coordinates, properties, ccp, source, nodeId)
    with BioEntity
    with DNA {

    override def getLabels = List("Gene", "BioEntity", "Feature", "DNA")

    def getName = name

    def getProduct = throw new Exception("Not implemented yet!")

    def controlledBy = throw new Exception("Not implemented yet!")

    def getTerms = terms

    def getSeq = sequence

    override def equals(that: Any): Boolean = that match {
      case that: Gene =>
        (that canEqual this) &&
        this.getCoordinates == that.getCoordinates &&
        this.getCCP == that.getCCP &&
        this.getOrganism == that.getOrganism
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Gene]

    override def hashCode = 41 * (41 + coordinates.hashCode) + sequence.hashCode

    def getOrganism = organism

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      val newProperties = this.setProperties(Map("name" -> this.getName))
  //        "start" -> this.getCoordinates.start,
  //        "end" -> this.getCoordinates.end,
  //        "strand" -> this.getCoordinates.strand.toString))
      val geneNode = super.upload(graphDataBaseConnection)
      newProperties.foreach{case (k, v) => geneNode.setProperty(k, v)}
      val termNodes = this.getTerms.map(_.upload(graphDataBaseConnection))
      termNodes.foreach(geneNode.createRelationshipTo(_, BiomeDBRelations.hasName))
      geneNode.createRelationshipTo(graphDataBaseConnection.getNodeById(this.getOrganism.getId), BiomeDBRelations.partOf)
      geneNode.createRelationshipTo(graphDataBaseConnection.getNodeById(this.getCCP.getId), BiomeDBRelations.partOf)
      geneNode.createRelationshipTo(graphDataBaseConnection.getNodeById(this.getSeq.getId), BiomeDBRelations.isA)
      geneNode
    }

  }

  case class Terminator(
                         coordinates: Coordinates,
                         ccp: CCP,
                         source: List[String],
                         properties: Map[String, Any] = Map(),
                         nodeId: Long = -1)
    extends Feature(coordinates, properties, ccp, source, nodeId)
    with DNA {

    override def getLabels = List("Terminator", "Feature", "DNA")

  }

  case class Promoter(name: String,
                      coordinates: Coordinates,
                      ccp: CCP,
                      organism: Organism,
                      tss: Int,
                      term: Term,
                      source: List[String],
                      properties: Map[String, Any] = Map(),
                      nodeId: Long = -1)
    extends Feature(coordinates, properties, ccp, source, nodeId)
    with FunctionalRegion
    with DNA {

    override def getLabels = List("Promoter", "BioEntity", "Feature", "DNA")

    def getName = name

    def getStandardName = term

    def getRegulationType = throw new Exception("Not implemented yet!")

    def getOrganism = organism
  }

  case class MiscFeature(miscFeatureType: String,
                         coordinates: Coordinates,
                         ccp: CCP,
                         source: List[String],
                         properties: Map[String, Any] = Map(),
                         nodeId: Long = -1)
    extends Feature(coordinates, properties, ccp, source, nodeId)
    with DNA {

    override def getLabels = List(miscFeatureType, "Feature", "DNA")
  }

  case class MobileElement(
                            name: String,
                            coordinates: Coordinates,
                            ccp: CCP,
                            source: List[String],
                            properties: Map[String, Any] = Map(),
                            nodeId: Long = -1)
    extends Feature(coordinates, properties, ccp, source, nodeId)
    with BioEntity
    with DNA {

    override def getLabels = List("Mobile_element", "Feature", "BioEntity", "DNA")

    def getName = name
  }

  case class Operon(
                     name: String,
                     boundaries: Boundaries,
                     term: Term,
                     organism: Organism,
                     properties: Map[String, Any] = Map(),
                     var tus: List[TU] = List(),
                     nodeId: Long = -1)
    extends Node(properties, nodeId)
    with BioEntity
    with DNA {
    //  def getCoordinates = List(properties("first_gene_position"), properties("last_gene_position "), properties("strand"))

    def getLabels = List("Operon", "BioEntity", "DNA")

    def getName = name

    def getOrganism = organism

    def getTUs = tus

    def nextOperon = throw new Exception("Not implemented yet!")

    def overlapedOperons = throw new Exception("Not implemented yet!")

    def getStandardName = term

    def addTU(tu: TU): Unit = {
      val newTus = List(tu) ::: tus
      tus = newTus
    }
  }

  case class TU(
                 name: String,
                 term: Term,
                 operon: Operon,
                 promoter: Promoter,
                 organism: Organism,
                 composition: List[Feature],
                 properties: Map[String, Any] = Map(),
                 nodeId: Long = -1)
    extends Node(properties, nodeId)
    with BioEntity
    with DNA {

    def getLabels = List("TU", "BioEntity", "DNA")

    def getName = name

    def consistsOf = List(promoter) ::: composition

    def getStandardName = term

    def participatesIn = throw new Exception("Not implemented yet!")

    def getOperon = operon

    def getOrganism = organism
  }

  case class Chromosome(
                         name: String,
                         source: List[String] = List("GenBank"),
                         dnaType: DNAType.Value = DNAType.unknown,
                         organism: Organism,
                         length: Int = -1,
                         properties: Map[String, Any] = Map(),
                         nodeId: Long = -1)
    extends Node(properties, nodeId)
    with CCP {

    def getLength = length

    def getChromType = dnaType

    def getType = CCPType.Chromosome

    def getSource = source

    def getName = name

    def getOrganism = organism

    def getLabels = List("Chromosome", "BioEntity")

    override def equals(that: Any): Boolean = that match {
      case that: Chromosome =>
        (that canEqual this) &&
          this.getName == that.getName &&
          this.getOrganism == that.getOrganism
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Chromosome]

    override def hashCode: Int = 41 * (41 + name.hashCode) + organism.hashCode

  //  override def createRelationshipToOrganism(
  //                                             graphDataBaseConnection: GraphDatabaseService,
  //                                             organismNode: graphdb.Node) = transaction(graphDataBaseConnection){
  //    val partOfRelationship = organismNode.c
  //  }
  //  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
  //    val newProperties = this.setProperties(Map("length" -> this.getLength, "circular" -> this.getChromType, "name" -> this.getName))
  //    val chromosomeNode = super.upload(graphDataBaseConnection)
  //    val organismNode = this.getOrganism.upload(graphDataBaseConnection)
  //    chromosomeNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
  //    newProperties.foreach{case (k, v) => chromosomeNode.setProperty(k, v)}
  //    chromosomeNode
  //  }

  //  def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection){
  //    val chromosomeNode = graphDataBaseConnection.createNode
  //    //    convert string to labels and add them to the node
  //    getLabels.map(utilFunctionsObject.stringToLabel).foreach(chromosomeNode.addLabel)
  //    //    upload properties from the Map "properties"
  //    (Map("source" -> getSource) ++ properties).foreach{case (k, v) => chromosomeNode.setProperty(k, v)}
  //    chromosomeNode
  //  }
  }

  case class Plasmid(
                      name: String,
                      source: List[String] = List("GenBank"),
                      dnaType: DNAType.Value = DNAType.unknown,
                      organism: Organism,
                      length: Int = -1,
                      properties: Map[String, Any] = Map(),
                      nodeId: Long = -1)
    extends Node(properties, nodeId)
    with CCP {

    def getLength = length

    def getChromType = dnaType

    def getType = CCPType.Plasmid

    def getSource = source

    def getName = name

    def getLabels = List("Plasmid", "BioEntity")

    def getOrganism = organism

    override def equals(that: Any): Boolean = that match {
      case that: Plasmid =>
        (that canEqual this) &&
          this.getName == that.getName &&
          this.getOrganism == that.getOrganism
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Plasmid]

    override def hashCode: Int = 41 * (41 + name.hashCode) + organism.hashCode

  //  def upload(
  //              graphDataBaseConnection: GraphDatabaseService,
  //              organismNode: graphdb.Node): graphdb.Node = transaction(graphDataBaseConnection) {
  //    val plasmidNode = super.upload(graphDataBaseConnection)
  //    plasmidNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
  //    plasmidNode
  //  }
  }

  case class Contig(
                     name: String,
                     source: List[String] = List("GenBank"),
                     dnaType: DNAType.Value = DNAType.unknown,
                     organism: Organism,
                     length: Int = -1,
                     properties: Map[String, Any] = Map(),
                     nodeId: Long = -1)
    extends Node(properties, nodeId)
    with CCP {

    def getLength = length

    def getChromType = dnaType

    def getType = CCPType.Contig

    def getSource = source

    def getName = name

    def getLabels = List("Contig", "BioEntity")

    def getOrganism = organism

    override def equals(that: Any): Boolean = that match {
      case that: Contig =>
        (that canEqual this) &&
          this.getName == that.getName &&
          this.getOrganism == that.getOrganism
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Contig]

    override def hashCode: Int = 41 * (41 + name.hashCode) + organism.hashCode

  //  def upload(
  //              graphDataBaseConnection: GraphDatabaseService,
  //              organismNode: graphdb.Node): graphdb.Node = transaction(graphDataBaseConnection) {
  //    val contigNode = super.upload(graphDataBaseConnection)
  //    contigNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
  //    contigNode
  //  }
  }

  case class Term(
                   text: String,
                   nodeId: Long = -1)
    extends Node(Map(), nodeId) {

    def getText = text

    def getLabels = List("Term")

    override def equals(that: Any) = that match {
      case that: Term =>
        (that canEqual this) &&
        this.getText == that.getText
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Term]

    override def hashCode = 41 * text.hashCode

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
//      val newProperties = this.setProperties(Map("text" -> this.getText))
//      val termNode = super.upload(graphDataBaseConnection)
//      newProperties.foreach{case (k, v) => termNode.setProperty(k, v)}
//      termNode

      if (this.getId < 0) {
        val newProperties = this.setProperties(Map("text" -> this.getText))
        val termNode = super.upload(graphDataBaseConnection)
        newProperties.foreach{case (k, v) => termNode.setProperty(k, v)}
        termNode
      }
      else graphDataBaseConnection.getNodeById(this.getId)

      }
  }

  case class Organism(
                       name: String,
                       source: List[String],
                       accessions: List[XRef] = List(),
                       var taxon: Taxon = Taxon("Empty", TaxonType.no_rank),
                       properties: Map[String, Any] = Map(),
                       nodeId: Long = -1)
    extends Node(properties, nodeId) {

    def getLabels = List("Organism")

    def getName = name

    override def equals(that: Any) = that match {
      case that: Organism =>
        (that canEqual this) &&
        this.getName == that.getName
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Organism]

    override def hashCode = 41 * name.hashCode

    def getTaxon = taxon

    def setTaxon(newTaxon: Taxon): Unit = taxon = newTaxon

    def getSource = source

    def getAccessions = accessions

    def setAccessions(newAccessions: List[String]) = accessions :: newAccessions

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
        val findOrganismNode = graphDataBaseConnection.findNode(DynamicLabel.label("Organism"), "name", this.getName)
        if (findOrganismNode == null) {
          val organismNode = super.upload(graphDataBaseConnection)
          this.setProperties(
            Map("source" -> this.getSource.mkString(", "), "name" -> this.getName))
            .foreach { case (k, v) => organismNode.setProperty(k, v) }
          val xrefNodes = this.getAccessions.map(_.upload(graphDataBaseConnection))
          xrefNodes.foreach(organismNode.createRelationshipTo(_, BiomeDBRelations.evidence))
          organismNode
        }
        else findOrganismNode

//        if (findOrganismNode.isInstanceOf[Node]) findOrganismNode
//        else {
//          val organismNode = super.upload(graphDataBaseConnection)
//          this.setProperties(
//            Map("source" -> this.getSource.mkString(", "), "name" -> this.getName))
//            .foreach { case (k, v) => organismNode.setProperty(k, v) }
//          organismNode
//        }
    }
  }

  case class Polypeptide(
                          name: String,
                          xRefs: List[XRef],
                          sequence: SequenceAA,
                          terms: List[Term],
                          gene: Gene,
                          organism: Organism,
                          source: List[String] = List("GenBank"),
                          properties: Map[String, Any] = Map(),
                          var geneNode: graphdb.Node = null,
                          nodeId: Long = -1)
    extends Node(properties, nodeId)
    with BioEntity with GeneProduct{

    def getName = name

    def getLabels = List("Polypeptide", "Peptide", "BioEntity")

    def getGene = gene

    def getOrganism = organism

    def getSeq = sequence

    def getTerms = terms

    def getXrefs = xRefs

    def getSource = source

    override def equals(that: Any) = that match {
      case that: Polypeptide =>
        (that canEqual this) &&
        this.getSeq == that.getSeq &&
        this.getOrganism == that.getOrganism &&
        this.getName == that.getName
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Polypeptide]

    override def hashCode = 41 * (41 * (41 + sequence.hashCode) + organism.hashCode) + name.hashCode

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      if (this.getId > -1) graphDataBaseConnection.getNodeById(this.getId)
      else {
        val newGeneAndPolypeptideProperties = this.setProperties(
          Map(
            "name" -> this.getName,
            "source" -> this.getSource.mkString(", ")))
        val polypeptideNode = super.upload(graphDataBaseConnection)
        newGeneAndPolypeptideProperties.foreach { case (k, v) => polypeptideNode.setProperty(k, v) }

        //      val sequenceNode = this.getSeq.upload(graphDataBaseConnection)

        val xrefNodes = this.getXrefs.map(_.upload(graphDataBaseConnection))

        val geneNode = graphDataBaseConnection.getNodeById(this.getGene.getId)
        xrefNodes.foreach(geneNode.createRelationshipTo(_, BiomeDBRelations.evidence))
        xrefNodes.foreach(polypeptideNode.createRelationshipTo(_, BiomeDBRelations.evidence))

        geneNode.createRelationshipTo(polypeptideNode, BiomeDBRelations.encodes)

        val termNodes = this.getTerms.map(_.upload(graphDataBaseConnection))
        termNodes.foreach(polypeptideNode.createRelationshipTo(_, BiomeDBRelations.hasName))

        polypeptideNode.createRelationshipTo(graphDataBaseConnection.getNodeById(this.getSeq.getId), BiomeDBRelations.isA)

        polypeptideNode.createRelationshipTo(graphDataBaseConnection.getNodeById(this.getOrganism.getId), BiomeDBRelations.partOf)

        polypeptideNode
      }
    }
  }

  abstract class Sequence(sequence: String,
                          md5: String,
                          similarities: List[Similarity],
                          toCheck: Boolean = false,
                          properties: Map[String, Any] = Map(),
                          nodeId: Long = -1)
    extends Node(properties, nodeId) {

    require (sequence.forall(_.isUpper), "Sequence must be written in upper case: " + sequence)

//    if (md5.length < 32) md5 = countMD5

    def getSequence = sequence

    def getLength: Int = sequence.length

    def getMD5: String

    def getSimilarities: List[Similarity]

    def countMD5 = utilFunctionsObject.md5ToString(sequence)

    def addSimilarity(similarity: Similarity): Unit

  }

  case class SequenceAA(
                       sequence: String,
                       var md5: String = "",
                       var similarities: List[Similarity] = List(),
                       toCheck: Boolean = false,
                       manuallyTranslated: Boolean = false,
                       properties: Map[String, Any] = Map(),
                       nodeId: Long = -1)
    extends Sequence(sequence, md5, similarities, toCheck, properties, nodeId) {

//    require (start <= end, "Start coordinate cannot have bigger value than end coordinate!")

    if (md5.length < 32) md5 = countMD5

    def getLabels = List("Sequence", "AA_Sequence")

    override def getMD5: String = md5

    override def equals(that: Any) = that match {
      case that: SequenceAA =>
        (that canEqual this) &&
        this.getMD5 == that.getMD5
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[SequenceAA]

    override def hashCode: Int = {
      41 * md5.hashCode
    }

    def getSimilarities = similarities

    def addSimilarity(similarity: Similarity): Unit = {
      if (!similarities.contains(similarity)) {
        val newSimilarity = List(similarity) ::: similarities
        similarities = newSimilarity
        similarity.getSequence.addSimilarity(Similarity(this, similarity.getEvalue, similarity.getIdentity))
      }
    }

    private def manuallyTranslatedToString: String = {
      manuallyTranslated match {
        case true => "manual"
        case false => "automatic"
      }
    }

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {

     if (this.getId < 0) {
          val newProperties = this.setProperties(Map(
            "md5" -> this.getMD5,
            "seq" -> this.getSequence,
            "translation" -> this.manuallyTranslatedToString,
            "length" -> this.getLength
          )
          )
          val sequenceNode = super.upload(graphDataBaseConnection)
          newProperties.foreach{case (k, v) => sequenceNode.setProperty(k, v)}
          sequenceNode
      }
      else graphDataBaseConnection.getNodeById(this.getId)
    }

  }

  case class SequenceDNA(
                          sequence: String,
                          var md5: String = "",
                          var similarities: List[Similarity] = List(),
                          translatable: Boolean = true,
                          properties: Map[String, Any] = Map(),
                          nodeId: Long = -1)
    extends Sequence(sequence, md5, similarities, translatable, properties, nodeId) {

    if (md5.length < 32) md5 = countMD5

    def getLabels = translatable match{
      case true => List("Sequence", "DNA_Sequence")
      case false => List("Sequence", "DNA_Sequence", "Untranslatable")
    }

    override def getMD5: String = md5

    override def equals(that: Any) = that match {
      case that: SequenceDNA =>
        (that canEqual this) &&
          this.getMD5 == that.getMD5
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[SequenceDNA]

    override def hashCode: Int = {
      41 * md5.hashCode
    }

    def getSimilarities = similarities

    def addSimilarity(similarity: Similarity): Unit = {
      if (!similarities.contains(similarity)) {
        val newSimilarity = List(similarity) ::: similarities
        similarities = newSimilarity
        similarity.getSequence.addSimilarity(Similarity(this, similarity.getEvalue, similarity.getIdentity))
      }
    }

    private def translatableToString: String = {
      translatable match {
        case true => "translatable"
        case false => "non-translatable"
      }
    }

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {

      if (this.getId < 0) {
        val newProperties = this.setProperties(Map(
          "md5" -> this.getMD5,
          "seq" -> this.getSequence,
          "translatable" -> this.translatableToString,
          "length" -> this.getLength
        )
        )
        val sequenceNode = super.upload(graphDataBaseConnection)
        newProperties.foreach{case (k, v) => sequenceNode.setProperty(k, v)}
        sequenceNode
      }
      else graphDataBaseConnection.getNodeById(this.getId)
    }


  }

  case class Taxon(
                    name: String,
                    taxonType: TaxonType.Value,
                    taxID: Int = -1,
                    nodeId: Long = -1)
    extends Node(properties = Map(), nodeId){

    def getLabels = List("Taxon")

    def getTaxID = taxID

    def getTaxonType = taxonType
  }

  case class Compound(
                     name: String,
                     inchi: String = "",
                     smiles: String = "",
                     var reference: List[XRef] = List(),
                     toCheck: Boolean = false,
                     nodeId: Long = -1)
    extends Node(properties = Map(), nodeId)
    with BioEntity{

    def getLabels = if (toCheck)
      List("Compound", "BioEntity", "To_check")
    else
      List("Compound", "BioEntity")

    def getName = name

    def getInchi = inchi

    def getSmiles = smiles

    def getXrefs = reference

    def setXrefs(newXref: XRef): Unit = reference = List(newXref) ::: reference

    override def equals(that: Any): Boolean = that match {
      case that: Compound =>
        (that canEqual this) &&
        this.getInchi == that.getInchi
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Compound]

    override def hashCode = 41 * inchi.hashCode

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      if (this.getId < 0) {
        val tryToFindNode = Option(graphDataBaseConnection.findNode(DynamicLabel.label("Compound"), "name", this.getName))
        val compoundNode = tryToFindNode match {
          case Some(n) => n
          case None =>
            val createdCompoundNode = super.upload(graphDataBaseConnection)
            this.setProperties(Map("name" -> this.getName)).foreach{case (k, v) => createdCompoundNode.setProperty(k, v)}
            createdCompoundNode
        }
        compoundNode
      }
      else graphDataBaseConnection.getNodeById(this.getId)
    }

  }

  object Compound {
    def apply(node: org.neo4j.graphdb.Node): Compound = {
      val props = node.getAllProperties
      Compound(
        name = props.getOrDefault("name", "").toString,
        inchi = props.getOrDefault("inchi", "").toString,
        smiles = props.getOrDefault("smiles", "").toString,
        nodeId = node.getId
      )
    }
  }

  case class RNA(
                name: String,
                gene: Gene,
                organism: Organism,
                rnaType: String,
                xRefs: List[XRef],
                source: List[String] = List("GenBank"),
                nodeId: Long = -1,
                properties: Map[String, Any] = Map()
                )
    extends Node(properties, nodeId)
    with BioEntity with GeneProduct {
    def getLabels = List("RNA", "BioEntity", rnaType)

    def getName = name

    def getSource = source

    def getOrganism = organism

    def getGene = gene

    def getXrefs = xRefs

    override def equals(that: Any): Boolean = that match {
      case that: RNA =>
        (that canEqual this) &&
          this.getOrganism == that.getOrganism &&
          this.getName == that.getName
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[RNA]

    override def hashCode = 41 * (41 + organism.hashCode) + name.hashCode

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      val newProperties = this.setProperties(Map("name" -> this.getName, "source" -> this.getSource.mkString(", ")))
      val rnaNode = super.upload(graphDataBaseConnection)
      newProperties.foreach{case (k, v) => rnaNode.setProperty(k, v)}

//      val geneNode = this.getGene.upload(graphDataBaseConnection)
      val geneNode = graphDataBaseConnection.getNodeById(this.getGene.getId)

      val xrefNodes = this.getXrefs.map(_.upload(graphDataBaseConnection))

      xrefNodes.foreach(geneNode.createRelationshipTo(_, BiomeDBRelations.evidence))
      geneNode.createRelationshipTo(rnaNode, BiomeDBRelations.encodes)

      rnaNode.createRelationshipTo(graphDataBaseConnection.getNodeById(this.getOrganism.getId), BiomeDBRelations.partOf)

      rnaNode
    }
  }

  case class Reactant(
                  name: String,
                  sequence: String = "",
                  inchi: Map[String, String] = Map(),
                  var stoichiometry: Option[Double] = None,
                  compartment: Option[Compartment] = None,
                  compounds: List[Compound] = List(),
                  formula: Option[String] = None,
                  charge: Option[Int] = None,
                  toCheck: Boolean = false,
                  properties: Map[String, Any] = Map(),
                  nodeId: Long = -1)
  extends Node(properties = properties, nodeId) {

    def getLabels = this.toCheck match {
      case true => List("Reactant", "To_check")
      case false => List("Reactant")
    }

    def getToCheck = this.toCheck

    def getName = this.name

    def getSequence = this.sequence

    def getInchi = this.inchi

    def getStoichiometry = this.stoichiometry

    def getCompartment = this.compartment

    def getCompounds = this.compounds

    def getCharge = this.charge

    def getFormula = this.formula

    def setStoichiometry(stoi: Option[Double]) = stoichiometry = stoi

    override def equals(that: Any): Boolean = that match {
      case that: Reactant =>
        (that canEqual this) &&
          this.getName == that.getName
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[Reactant]

    override def hashCode = 41 * name.hashCode

    override def upload(graphDatabaseConnection: GraphDatabaseService): graphdb.Node = {
      if (this.getId < 0) {
        var newProperties = this.getSequence.nonEmpty match {
          case true => this.setProperties(Map("name" -> this.getName, "seq" -> this.getSequence) ++ this.getInchi)
          case false => this.setProperties(Map("name" -> this.getName) ++ this.getInchi)
        }

        newProperties = this.getCharge match {
          case Some(c) => newProperties ++ Map("charge" -> c)
          case None => newProperties
        }

        newProperties = this.getFormula match {
          case Some(f: String) => newProperties ++ Map("chemical_formula" -> f)
          case Some(null) => newProperties
          case None => newProperties
        }

        val reactantNode = super.upload(graphDatabaseConnection)
        newProperties.foreach { case (k, v) => reactantNode.setProperty(k, v) }

        this.getCompartment match {
          case Some(c) =>
            val compartmentNode = c.upload(graphDatabaseConnection)
            reactantNode.createRelationshipTo(compartmentNode, BiomeDBRelations.locates_in)
          case None =>
        }

        val compoundNodes = compounds.map(_.upload(graphDatabaseConnection))
        compoundNodes.foreach(reactantNode.createRelationshipTo(_, BiomeDBRelations.isA))

        reactantNode
      }
      else graphDatabaseConnection.getNodeById(this.getId)
    }
  }

  case class BiochemicalReaction(name: String,
                                 reactants: List[Reactant],
                                 products: List[Reactant] = List(),
                                 organism: Option[Organism] = None,
                                 xRefs: List[XRef] = List(),
                                 enzymes: List[Enzyme] = List(),
                                 experiment: String = "",
                                 properties: Map[String, Any] = Map(),
                                 isSpontaneous: Boolean = false,
                                 nodeId: Long = -1) extends Node(properties = properties, nodeId) {
    def getName = this.name

    def getLabels = "BiochemicalReaction" :: (if (!isSpontaneous) Nil else "SpontaneousReaction" :: Nil)

    def getExperiment = this.experiment

    def getXrefs = this.xRefs

    def getReactants = this.reactants

    def getProducts = this.products

    override def equals(that: Any): Boolean = that match {
      case that: BiochemicalReaction =>
        (that canEqual this) &&
          this.getName == that.getName
      case _ => false
    }

    override def canEqual(that: Any) = that.isInstanceOf[BiochemicalReaction]

    override def hashCode = 41 * name.hashCode

    override def upload(db: GraphDatabaseService): graphdb.Node = {
      val newProperty = this.getExperiment match {
        case e: String => this.setProperties(Map("name" -> this.getName, "experiment" -> e))
        case null => this.setProperties(Map("name" -> this.getName))
      }

      val biochemReactionNode = super.upload(db)
      newProperty.foreach{case (k, v) => biochemReactionNode.setProperty(k, v)}

      val xrefNodes = this.getXrefs.map(_.upload(db))
      xrefNodes.foreach(biochemReactionNode.createRelationshipTo(_, BiomeDBRelations.evidence))

      //link reactants
      val linkToReaction: (Reactant, Boolean) => Unit = createRelationshipsToReactants(db, biochemReactionNode, _, _)
      reactants.foreach(linkToReaction(_, false))
      products.foreach(linkToReaction(_, true))

      //link organism
      organism.map(organism => createPartOfRelationship(biochemReactionNode, db.getNodeById(organism.getId)))

      val chemReaction = getOrCreateChemicalReaction(reactants, products, name, db)
      biochemReactionNode.createRelationshipTo(db.getNodeById(chemReaction.getId), BiomeDBRelations.isA)

      biochemReactionNode
    }

    private def createChemicalReaction(reactantsCompounds: List[org.neo4j.graphdb.Node],
                                       productsCompounds: List[org.neo4j.graphdb.Node],
                                       db: GraphDatabaseService): ChemicalReaction = {
      val req = reactantsCompounds.nonEmpty || productsCompounds.nonEmpty
      require(req,
        "reactants and products for a ChemicalReaction are empty")

      val chemReaction = ChemicalReaction(
        reactants = reactantsCompounds.map(Compound.apply),
        products = productsCompounds.map(Compound.apply)
      )

      chemReaction.upload(db)

      chemReaction
    }


    private def getOrCreateChemicalReaction(reactants: List[Reactant],
                                            products: List[Reactant],
                                            reactionName: String,
                                            db: GraphDatabaseService): ChemicalReaction = {

      val reactantsCompounds = findCompounds(reactants, db)
      val reactantsCompoundsIds = HashSet(reactantsCompounds.map(_.getId):_*)
      val reactantsChemicalReactions = findReactantsReactions(reactantsCompounds)

      val productsCompounds = findCompounds(products, db)
      val productsCompoundsIds = HashSet(productsCompounds.map(_.getId):_*)
      val productsChemicalReactions = findProductsReactions(productsCompounds)

      val reactionsToConsider = if (products.nonEmpty) {
        val productsChemicalReactionsIds = productsChemicalReactions.map(_.getId)
        reactantsChemicalReactions.filter(rcr => productsChemicalReactionsIds.contains(rcr.getId))
      }
      else //transport reactions don't have products
        reactantsChemicalReactions

      reactionsToConsider
        .find { chemReactionNode =>
          //reaction with the same reactants and products
          findReactionReactants(chemReactionNode).forall(r => reactantsCompoundsIds.contains(r.getId)) &&
            findReactionProducts(chemReactionNode).forall(r => productsCompoundsIds.contains(r.getId))
        }
        .map { chemReactionNode =>
          val chemReactants = reactantsCompounds.map(Compound.apply)
          val chemProducts = productsCompounds.map(Compound.apply)
          ChemicalReaction(
            chemReactants,
            chemProducts,
            chemReactionNode.getId
          )
        }
        .getOrElse(createChemicalReaction(reactantsCompounds, productsCompounds, db))
    }

    private def findReactionReactants(reaction: org.neo4j.graphdb.Node): Set[org.neo4j.graphdb.Node] = {
      reaction.getRelationships(INCOMING, is_reactant).asScala.map(_.getStartNode).toSet
    }
    private def findReactionProducts(reaction: org.neo4j.graphdb.Node): Set[org.neo4j.graphdb.Node] = {
      reaction.getRelationships(OUTGOING, is_product).asScala.map(_.getEndNode).toSet
    }

    private def findProductsReactions(compounds: List[org.neo4j.graphdb.Node]): Set[org.neo4j.graphdb.Node] = {
      compounds.flatMap { compound =>
        compound
          .getRelationships(INCOMING, is_product).asScala
          .map(_.getStartNode)
      }.toSet
    }

    private def findReactantsReactions(compounds: List[org.neo4j.graphdb.Node]): Set[org.neo4j.graphdb.Node] = {
      compounds.flatMap { compound =>
        compound
          .getRelationships(OUTGOING, is_reactant).asScala
          .map(_.getEndNode)
      }.toSet
    }

    private def findCompounds(metabolites: List[Reactant], db: GraphDatabaseService) = {
      metabolites.flatMap { metabolite =>
        db.getNodeById(metabolite.getId)
          .getRelationships(Direction.OUTGOING, BiomeDBRelations.isA).asScala
          .headOption
          .map(_.getEndNode)
      }
    }

    private def createPartOfRelationship(reactionNode: org.neo4j.graphdb.Node,
                                         organismNode: org.neo4j.graphdb.Node): Relationship = {
      reactionNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
    }

    private def createRelationshipsToReactants(db: GraphDatabaseService,
                                               reactionNode: org.neo4j.graphdb.Node,
                                               reactant: Reactant,
                                               productFlag: Boolean): Unit = {
      val reactantNode = db.getNodeById(reactant.getId)
      val tryToFindReaction = utilFunctionsObject.findExistingRelationship(db,
        reactantNode,
        reactionNode,
        Direction.OUTGOING,
        BiomeDBRelations.participates_in)
      if (tryToFindReaction.nonEmpty) {
        val foundRelNodePair = tryToFindReaction.head
        foundRelNodePair._1.setProperty("N", foundRelNodePair._1.getProperty("N").toString.toInt + 1)
      } else {
        val participationRelationship = reactantNode.createRelationshipTo(reactionNode, BiomeDBRelations.participates_in)

        val reactantRelationship = if (productFlag)
          reactionNode.createRelationshipTo(reactantNode, BiomeDBRelations.is_product)
        else
          reactantNode.createRelationshipTo(reactionNode, BiomeDBRelations.is_reactant)

        participationRelationship.setProperty("N", 1)
        reactant.getStoichiometry match {
          case Some(stoi) => reactantRelationship.setProperty("stoichiometric_coef", stoi)
          case None =>
        }
      }
    }
  }

  case class ChemicalReaction(reactants: List[Compound],
                              products: List[Compound] = List(),
                              private val nodeId: Long = -1) extends Node(properties = Map(), nodeId) {
    override def getLabels: List[String] = List("ChemicalReaction")

    override def upload(db: GraphDatabaseService): org.neo4j.graphdb.Node = {
      val reactionNode = super.upload(db)

      reactants.foreach { reactant =>
        db.getNodeById(reactant.getId).createRelationshipTo(reactionNode, is_reactant)
      }
      products.foreach { product =>
        reactionNode.createRelationshipTo(db.getNodeById(product.getId), is_product)
      }

      reactionNode
    }
  }

  case class Compartment(
                          name: String,
                          organism: Organism,
                          nodeId: Long = -1
                        )
  extends Node(Map(), nodeId) with BioEntity{

    def getName = name

    def getLabels = List("Compartment", "BioEntity")

    override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = {
      if (this.getId < 0) {
        val compartmentNode = findOrganismCompartment(graphDataBaseConnection) match {
          case Some(n) => n
          case None => createCompartmentNode(graphDataBaseConnection)
        }
        compartmentNode
      }
      else graphDataBaseConnection.getNodeById(this.getId)
    }

    private def findOrganismCompartment(db: GraphDatabaseService): Option[org.neo4j.graphdb.Node] = {
      val foundCompartments = db.findNodes(DynamicLabel.label("Compartment"), "name", this.getName)
        .asScala
        .flatMap{cn =>
          cn.getRelationships(BiomeDBRelations.partOf, Direction.OUTGOING)
            .asScala
            .map(_.getEndNode)}
      val filtered = foundCompartments.filter(n => n.getProperty("name").toString == organism.name)

      if (filtered.nonEmpty) Option(filtered.next)
      else None
    }

    private def createCompartmentNode(graphDataBaseConnection: GraphDatabaseService) = {
      val props = Map(
        "name" -> this.getName,
        "organismName" -> organism.name
      )

      val createdCompartmentNode = super.upload(graphDataBaseConnection)
      this.setProperties(props).foreach { case (k, v) => createdCompartmentNode.setProperty(k, v) }

      createPartOfRelationship(createdCompartmentNode, graphDataBaseConnection.getNodeById(organism.getId))

      createdCompartmentNode
    }

    private def createPartOfRelationship(createdCompartmentNode: org.neo4j.graphdb.Node,
                                         organismNode: org.neo4j.graphdb.Node): Relationship = {
      createdCompartmentNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
    }
  }


  case class Enzyme(
                     name: String,
                     var polypeptide: List[Polypeptide] = List(),
                     //                     var complex: List[Complex] = List(),
                     //                     var regulates: List[EnzymeRegulation] = List(),
                     var catalizes: List[BiochemicalReaction] = List(),
                     nodeId: Long = -1,
                     properties: Map[String, Any] = Map())
    extends Node(properties = properties, nodeId)
    with BioEntity{

    def getLabels = List("Enzyme", "Protein", "BioEntity")

    def getName = name

    def getPolypeptide = polypeptide

//    def getComplexes = complex

//    def getRegulations = regulates

    def getCatalization = catalizes

    def setPolypeptide(newPolypeptide: Polypeptide) = polypeptide ::: List(newPolypeptide)

//    def setComplexes(listOfComplexes: List[Complex]): Unit = complex ::: listOfComplexes

//    def setRegulations(newRegulatesList: List[EnzymeRegulation]): Unit = regulates ::: newRegulatesList
  }

//  case class Antiantitermintor(
//                              coordinates: Coordinates,
//                              ccp: CCP,
//                              var modulates: List[Terminator] = List(),
//                              var participatesIn: List[Attenuation],
//                              nodeId: BigInt = -1)
//    extends Feature(coordinates, properties = Map(), ccp, nodeId)
//    with DNA {
//
//    override def getLabels = List("Antiantitermintor", "Feature")
//
//    override def getCCP = ccp
//  }

  class LinkTo(start: XRef, end: DBNode, properties: Map[String, String] = Map()) extends Rel(id = -1, start, end, properties) {

    def getLabel = "LINK_TO"

  }

  class Evidence(start: Node, end: XRef, properties: Map[String, String] = Map()) extends Rel(id = -1, start, end, properties) {

    def getLabel = "EVIDENCE"

  }

  case class Similar(start: SequenceAA, end: SequenceAA, identity: Float, evalue: Double, relId: Long = -1) extends Rel(relId, start, end, Map()) {

    def getLabel = "SIMILAR"

    def getStart = start

    def getEnd = end

  }
}

