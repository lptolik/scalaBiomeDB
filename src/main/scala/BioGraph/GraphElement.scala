import BioGraph.{DBNode, Node, XRef, Sequence, Rel, BioEntity}
package BioGraph {

  import org.neo4j.graphdb
  import org.neo4j.graphdb.{Label, GraphDatabaseService}
  import utilFunctions._

/**
  * created by artem on 11.02.16.
  */


trait GraphElement {

  def getProperties: Map[String, Any]

  def isNode: Boolean

  def isRel: Boolean

  def getId: BigInt

  //  type Strand = String

}


abstract class Node(
                     properties: Map[String, Any],
                     var id: BigInt = -1)
  extends GraphElement with TransactionSupport  {

  def isNode = true

  def isRel = false

  def getProperties = properties

  def setProperties(newProperties: Map[String, Any]): Unit = properties ++ newProperties

  def getLabels: List[String]

  override def toString: String = getLabels.toString()

  def getId = id

  def setId(newId: BigInt): Unit = id = newId

  //  def outgoing: List[rel]
  //  def incoming: List[rel]

//  def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node
  def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
    val graphDBNode = graphDataBaseConnection.createNode
    //    convert string to labels and add them to the node
    getLabels.map(utilFunctionsObject.stringToLabel).foreach(graphDBNode.addLabel)
    //    upload properties from the Map "properties"
    properties.foreach{case (k, v) => graphDBNode.setProperty(k, v)}
    graphDBNode
  }
}

abstract class Rel(
                    id: BigInt = -1,
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

  def getName: String
}

trait DNA {

  //  def getCoordinates: Coordinates

}


trait GeneProduct{

  def getGene: Gene
}

trait CCP extends BioEntity {

  def getLength: Int

  def getType: CCPType.Value

  def getSource: ReferenceSource.Value

  def getChromType: DNAType.Value

  def getOrganism: Organism
}

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

case class DBNode(
                   name: String,
                   properties: Map[String, String] = Map(),
                   nodeId: BigInt = -1)
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
}

case class XRef(xrefId: String,
                dbNode: DBNode,
                properties: Map[String, String] = Map(),
                nodeId: BigInt = -1)
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

}

abstract class Feature(coordinates: Coordinates,
                       properties: Map[String, Any] = Map(),
                       ccp: CCP,
                       nodeId: BigInt = -1)
  extends Node(properties, nodeId) {

  def getCoordinates = coordinates

  def getLabels = List("Feature", "DNA")

  def next = throw new Exception("Not implemented yet!")

  def previous = throw new Exception("Not implemented yet!")

  def overlaps = throw new Exception("Not implemented yet!")

  def getCCP = ccp


//  override def equals(that: Any) = that match {
//    case that: Feature => this.getCCP == that.getCCP &&
//      this.getCoordinates == that.getCoordinates
//    case _ => false
//  }

//  def canEqual(that: Any): Boolean
}

case class Gene(
                 name: String,
                 coordinates: Coordinates,
                 ccp: CCP,
                 terms: List[Term],
                 organism: Organism,
                 properties: Map[String, Any] = Map(),
                 nodeId: BigInt = -1)
  extends Feature(coordinates, properties, ccp, nodeId)
  with BioEntity
  with DNA {

  override def getLabels = List("BioEntity", "Feature", "Gene")

  def getName = name

  def getProduct = throw new Exception("Not implemented yet!")

  def controlledBy = throw new Exception("Not implemented yet!")

  def getTerms = terms

  override def equals(that: Any): Boolean = that match {
    case that: Gene =>
      (that canEqual this) &&
      this.getCoordinates == that.getCoordinates &&
      this.getCCP == that.getCCP &&
      this.getOrganism == that.getOrganism
    case _ => false
  }

  override def canEqual(that: Any) = that.isInstanceOf[Gene]

  override def hashCode = 41 * coordinates.hashCode

  def getOrganism = organism

  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
    this.setProperties(
      Map(
        "name" -> this.getName,
        "start" -> this.getCoordinates.start,
        "end" -> this.getCoordinates.end,
        "strand" -> this.getCoordinates.strand))
    super.upload(graphDataBaseConnection)
  }
}

case class Terminator(
                       coordinates: Coordinates,
                       ccp: CCP,
                       properties: Map[String, Any] = Map(),
                       nodeId: BigInt = -1)
  extends Feature(coordinates, properties, ccp, nodeId)
  with DNA {

  override def getLabels = List("Terminator", "Feature", "DNA")

}

case class Promoter(name: String,
                    coordinates: Coordinates,
                    ccp: CCP,
                    organism: Organism,
                    tss: Int,
                    term: Term,
                    properties: Map[String, Any] = Map(),
                    nodeId: BigInt = -1)
  extends Feature(coordinates, properties, ccp, nodeId)
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
                       properties: Map[String, Any] = Map(),
                       nodeId: BigInt = -1)
  extends Feature(coordinates, properties, ccp, nodeId)
  with DNA {

  override def getLabels = List(miscFeatureType, "Feature", "DNA")
}

case class MobileElement(
                          name: String,
                          coordinates: Coordinates,
                          ccp: CCP,
                          properties: Map[String, Any] = Map(),
                          nodeId: BigInt = -1)
  extends Feature(coordinates, properties, ccp, nodeId)
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
                   nodeId: BigInt = -1)
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
    var newTus = List(tu) ::: tus
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
               nodeId: BigInt = -1)
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
                       source: ReferenceSource.Value = ReferenceSource.unknown,
                       dnaType: DNAType.Value = DNAType.unknown,
                       organism: Organism,
                       length: Int = -1,
                       properties: Map[String, Any] = Map(),
                       nodeId: BigInt = -1)
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
  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
    this.setProperties(Map("length" -> this.getLength, "circular" -> this.getChromType, "name" -> this.getName))
    val chromosomeNode = super.upload(graphDataBaseConnection)
    val organismNode = this.getOrganism.upload(graphDataBaseConnection)
    chromosomeNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
    chromosomeNode
  }

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
                    source: ReferenceSource.Value = ReferenceSource.unknown,
                    dnaType: DNAType.Value = DNAType.unknown,
                    organism: Organism,
                    length: Int = -1,
                    properties: Map[String, Any] = Map(),
                    nodeId: BigInt = -1)
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

  def upload(
              graphDataBaseConnection: GraphDatabaseService,
              organismNode: graphdb.Node): graphdb.Node = transaction(graphDataBaseConnection) {
    val plasmidNode = super.upload(graphDataBaseConnection)
    plasmidNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
    plasmidNode
  }
}

case class Contig(
                   name: String,
                   source: ReferenceSource.Value = ReferenceSource.unknown,
                   dnaType: DNAType.Value = DNAType.unknown,
                   organism: Organism,
                   length: Int = -1,
                   properties: Map[String, Any] = Map(),
                   nodeId: BigInt = -1)
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

  def upload(
              graphDataBaseConnection: GraphDatabaseService,
              organismNode: graphdb.Node): graphdb.Node = transaction(graphDataBaseConnection) {
    val contigNode = super.upload(graphDataBaseConnection)
    contigNode.createRelationshipTo(organismNode, BiomeDBRelations.partOf)
    contigNode
  }
}

case class Term(
                 text: String,
                 nodeId: Int = -1)
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

}

case class Organism(
                     name: String,
                     source: ReferenceSource.Value = ReferenceSource.unknown,
                     var taxon: Taxon = new Taxon("Empty", TaxonType.no_rank),
                     properties: Map[String, Any] = Map(),
                     nodeId: BigInt = -1)
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

  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
    this.setProperties(Map("source" -> source))
    val organismNode = super.upload(graphDataBaseConnection)
    organismNode
  }
}

case class Polypeptide(
                        name: String,
                        xRefs: List[XRef],
                        sequence: Sequence,
                        terms: List[Term],
                        gene: Gene,
                        organism: Organism,
                        source: List[String] = List("GenBank"),
                        properties: Map[String, Any] = Map(),
                        nodeId: Int = -1)
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

  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
    this.setProperties(Map("name" -> this.getName))
    val polypeptideNode = super.upload(graphDataBaseConnection)
    val geneNode = this.getGene.upload(graphDataBaseConnection)
    val sequenceNode = this.getSeq.upload(graphDataBaseConnection)
    geneNode.createRelationshipTo(polypeptideNode, BiomeDBRelations.encodes)
    polypeptideNode.createRelationshipTo(sequenceNode, BiomeDBRelations.isA)
    polypeptideNode
  }
}

case class Sequence(
                     sequence: String,
                     var md5: String = "",
                     var similarities: List[Similarity] = List(),
                     properties: Map[String, Any] = Map(),
                     nodeId: Int = -1)
  extends Node(properties, nodeId) {

  if (md5.length < 32) md5 = countMD5

  def getLabels = List("Sequence", "AA_Sequence")

  def getSequence = sequence

  def getMD5 = md5

  def getSimilarities = similarities

  def countMD5 = utilFunctionsObject.md5ToString(sequence)

  override def equals(that: Any) = that match {
    case that: Sequence =>
      (that canEqual this) &&
      this.getMD5 == that.getMD5
    case _ => false
  }

  override def canEqual(that: Any) = that.isInstanceOf[Sequence]

  override def hashCode: Int = {
    41 * md5.hashCode
  }

  def addSimilarity(similarity: Similarity): Unit = {
    if (!similarities.contains(similarity)) {
      var newSimilarity = List(similarity) ::: similarities
      similarities = newSimilarity
      similarity.getSequence.addSimilarity(new Similarity(this, similarity.getEvalue, similarity.getIdentity))
    }
  }

  override def upload(graphDataBaseConnection: GraphDatabaseService): graphdb.Node = transaction(graphDataBaseConnection) {
    this.setProperties(Map("md5" -> this.getMD5))
    super.upload(graphDataBaseConnection)
  }

//  override def toString = md5
}

case class Taxon(
                  name: String,
                  taxonType: TaxonType.Value,
                  taxID: Int = -1,
                  nodeId: BigInt = -1)
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
                   nodeId: BigInt = -1)
  extends Node(properties = Map(), nodeId)
  with BioEntity{

  def getLabels = List("Compound", "BioEntity")

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
}

case class RNA(
              name: String,
              gene: Gene,
              organism: Organism,
              rnaType: String,
              source: List[String] = List("GenBank"),
              nodeId: BigInt = -1
              )
  extends Node(properties = Map(), nodeId)
  with BioEntity with GeneProduct {
  def getLabels = List("RNA", "BioEntity", rnaType)

  def getName = name

  def getSource = source

  def getOrganism = organism

  def getGene = gene

  override def equals(that: Any): Boolean = that match {
    case that: RNA =>
      (that canEqual this) &&
        this.getOrganism == that.getOrganism &&
        this.getName == that.getName
    case _ => false
  }

  override def canEqual(that: Any) = that.isInstanceOf[RNA]

  override def hashCode = 41 * (41 + organism.hashCode) + name.hashCode
}

//case class Enzyme(
//                   name: String,
//                   var polypeptide: List[Polypeptide] = List(),
//                   var complex: List[Complex] = List(),
//                   var regulates: List[EnzymeRegulation] = List(),
//                   var catalizes: List[Reaction] = List(),
//                   nodeId: BigInt = -1)
//  extends Node(properties = Map(), nodeId)
//  with BioEntity{
//
//  def getLabels = List("Enzyme", "Protein", "BioEntity")
//
//  def getName = name
//
//  def getPolypeptide = polypeptide
//
//  def getComplexes = complex
//
//  def getRegulations = regulates
//
//  def getCatalization = catalizes
//
//  def setPolypeptide(newPolypeptide: Polypeptide) = polypeptide ::: List(newPolypeptide)
//
//  def setComplexes(listOfComplexes: List[Complex]): Unit = complex ::: listOfComplexes
//
//  def setRegulations(newRegulatesList: List[EnzymeRegulation]): Unit = regulates ::: newRegulatesList
//}
//
//case class Antiantitermintor(
//                            coordinates: Coordinates,
//                            ccp: CCP,
//                            var modulates: List[Terminator] = List(),
//                            var participatesIn: List[Attenuation],
//                            nodeId: BigInt = -1)
//  extends Feature(coordinates, properties = Map(), ccp, nodeId)
//  with DNA {
//
//  override def getLabels = List("Antiantitermintor", "Feature")
//
//  override def getCCP = ccp
//}

  class LinkTo(start: XRef, end: DBNode, properties: Map[String, String] = Map()) extends Rel(id = -1, start, end, properties) {

    def getLabel = "LINK_TO"

  }

  class Evidence(start: Node, end: XRef, properties: Map[String, String] = Map()) extends Rel(id = -1, start, end, properties) {

    def getLabel = "EVIDENCE"

  }

  case class Similar(start: Sequence, end: Sequence, identity: Float, evalue: Double, relId: BigInt = -1) extends Rel(relId, start, end, Map()) {

    def getLabel = "SIMILAR"

    def getStart = start

    def getEnd = end

  }

  trait TransactionSupport {

    protected def transaction[A <: Any](graphDataBaseConnection: GraphDatabaseService)(dbOperation: => A): A = {
      val tx = graphDataBaseConnection.beginTx()
      try {
        val result = dbOperation
        tx.success()
        result
      } finally {
        tx.close()
      }
    }
  }
}

