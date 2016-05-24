package BioGraph

import java.io.{PrintWriter, File}
import java.util
import org.biojava.nbio.core.exceptions.ParserException
import org.biojava.nbio.core.sequence.compound.{AmbiguityDNACompoundSet, NucleotideCompound}
import org.neo4j.graphdb.{GraphDatabaseService, DynamicLabel}
import utilFunctions.TransactionSupport

import scala.collection.JavaConverters._
import org.biojava.nbio.core.sequence.DNASequence
import org.biojava.nbio.core.sequence.io.{DNASequenceCreator, GenericGenbankHeaderParser, GenbankReader, GenbankReaderHelper}
import org.biojava.nbio.core.sequence.features.FeatureInterface
import org.biojava.nbio.core.sequence.template.AbstractSequence
import org.apache.logging.log4j.LogManager
import org.biojava.bio.seq.DNATools

import scala.collection.immutable.{HashMap, Map}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Created by artem on 25.04.16.
  */
class GenBankUtil(gbFile: File) extends TransactionSupport{
//  val geneList = List[Gene]()
  type NucleotideFeature = FeatureInterface[AbstractSequence[_root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound], _root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound]
  val genbankSourceValue = List("GenBank")
  val logger = LogManager.getLogger(this.getClass.getName)
  logger.info("Start processing " + gbFile.getName)
  var sequenceCollector: Map[String, Sequence] = Map()
  var externalDataBasesCollector: Map[String, DBNode] = Map()

  private def correctDBXrefInFile(gbFile: File): File = {
    val outputFileName = gbFile.getAbsolutePath.split(".gb")(0) + "_corrected_dbxrefs.gb"
    val outputFile = new PrintWriter(new File(outputFileName))
    def rewriteWithProperXRefs(line: String): Unit = {
      if (line.contains("/db_xref=") && line.split(":")(1).contains(" ")) outputFile.write(line.split(" ")(0) + "\"\n")
      else outputFile.write(line + "\n")
    }
    val fileReader = Source.fromFile(gbFile.getAbsolutePath)
    fileReader.getLines().foreach(rewriteWithProperXRefs)

    outputFile.close()
    fileReader.close()
    new java.io.File(outputFileName)
  }

  def getAccessionsFromGenBankFile: Map[String, DNASequence] = {
    val gbr = new GenbankReader[DNASequence, NucleotideCompound](
      gbFile, new GenericGenbankHeaderParser[DNASequence, NucleotideCompound](),
      new DNASequenceCreator(AmbiguityDNACompoundSet.getDNACompoundSet)
    )
    try {
      val dnaSequences = gbr.process()
      val accessions = HashMap() ++ dnaSequences.asScala
      accessions
    }
    catch {
      case e: ParserException =>
        logger.warn("Bad DBXref, rewriting the file " + gbFile.getName)
        val correctedGbr = new GenbankReader[DNASequence, NucleotideCompound](correctDBXrefInFile(gbFile), new GenericGenbankHeaderParser[DNASequence, NucleotideCompound](),
          new DNASequenceCreator(AmbiguityDNACompoundSet.getDNACompoundSet)
        )
        val dnaSequences = correctedGbr.process()
        val accessions = HashMap() ++ dnaSequences.asScala
        accessions
    }

//    val dnaSequences = GenbankReaderHelper.readGenbankDNASequence(gbFile)
//    val accessions = HashMap() ++ dnaSequences.asScala
//    accessions
  }

  def getInitialData(dnaSeq: DNASequence): (Organism, Node with CCP, DNASequence) = {
    val accession = dnaSeq.getAccession.toString
    val dnaLength = dnaSeq.getLength.toString
    val circularOrLinear = if (dnaSeq.getOriginalHeader.contains("circular")) DNAType.circular else DNAType.linear
    val ccpLength = dnaSeq.getLength
    val descriptionForUpload = dnaSeq.getDescription
    val description = descriptionForUpload.toUpperCase
    val ccpType =
      if (description.contains("COMPLETE GENOME") || description.contains("COMPLETE SEQUENCE")) "Chromosome"
      else if (description.contains("CONTIG")) "Contig"
      else if (description.contains("PLASMID")) "Plasmid"
      else {
        logger.warn("Unknown CCP type")
        "Contig"
      }
//    features(0).getQualifiers.get("organism").get(0).getValue
    val organismName = dnaSeq.getFeaturesByType("source").get(0).getQualifiers.get("organism").get(0).getValue
//    val organismName = dnaSeq.getFeatures.get(0).getQualifiers.get("organism").get(0).getValue
    val organism = new Organism(name = organismName, source = genbankSourceValue)

    val ccp = ccpType match{
      case "Chromosome" => new Chromosome(
        name = descriptionForUpload,
        organism = organism,
        dnaType = circularOrLinear,
        source = genbankSourceValue,
        length = ccpLength)
      case "Contig" => new Contig(
        name = descriptionForUpload,
        organism = organism,
        dnaType = circularOrLinear,
        source = genbankSourceValue,
        length = ccpLength)
      case "Plasmid" => new Plasmid(
        name = descriptionForUpload,
        organism = organism,
        dnaType = circularOrLinear,
        source = genbankSourceValue,
        length = ccpLength)
    }
    (organism, ccp, dnaSeq)
  }

  def getFeatures(dnaSeq: DNASequence): List[NucleotideFeature] = {
    val features = dnaSeq.getFeatures.asScala.toList
    features
  }

  def processFeatures(orgAndCCP: (Organism, Node with CCP, DNASequence))(listOfFeatures: List[NucleotideFeature]) = {
    listOfFeatures.map(processFeature(orgAndCCP: (Organism, Node with CCP, DNASequence))(_))
  }

  private def processFeature(orgAndCCP: (Organism, Node with CCP, DNASequence))(feature: NucleotideFeature) = feature.getType match {
//    case source => ???
//      Set(Set(misc_feature, repeat_region, source, rRNA, mobile_element, ncRNA, tRNA, tmRNA, CDS, gene, rep_origin, STS))
//    case "gene" => makeRNA(feature)
//    case "source" => makeCCPandOrganism(feature)
    case "CDS" => makeGenePolypeptideSequence(feature, orgAndCCP)
    case "tRNA" | "rRNA" | "ncRNA" | "tmRNA" => makeGeneAndRNA(feature, orgAndCCP, feature.getType)
    case "mobile_element" | "rep_origin" | "STS" | "misc_feature" | "repeat_region" =>
      makeMiscFeature(feature, feature.getType, orgAndCCP)
    case "gene" => makePseudoGene(feature, orgAndCCP)
    case "source" => //makeSourceNodes(feature, orgAndCCP)
    case _ => logger.warn("Unknown feature type: " + feature.getType + " in file " + gbFile.getName)
  }

  def makeMiscFeature(
                       miscFeature: NucleotideFeature,
                       miscFeatureType: String,
                       orgAndCCP: (Organism, Node with CCP, DNASequence)): (Feature with DNA) = {
    def getMiscFeatureAdditionalProperties: Try[String] = {
      Try(miscFeature.getQualifiers.get("note").get(0).getValue)
    }
    val note = getMiscFeatureAdditionalProperties
    val properties = note match {
      case Success(properNote) => Map[String, Any]("comment" -> properNote)
      case Failure(except) => Map[String, Any]()
    }

    val misc = new MiscFeature(
      miscFeatureType = miscFeatureType,
      coordinates = getCoordinates(miscFeature),
      ccp = orgAndCCP._2,
      source = genbankSourceValue,
      properties = properties
    )
    misc
  }

  def makePseudoGene(feature: NucleotideFeature, orgAndCCP: (Organism, Node with CCP, DNASequence)) = {
    if (feature.getQualifiers.containsKey("pseudo")) {
      val gene = makeGene(feature, orgAndCCP._1, orgAndCCP._2)
      gene
//      gene.copy(properties = Map("comment" -> "pseudo"))
    }
  }

  private def makeGene(feature: NucleotideFeature, organism: Organism, ccp: CCP): Gene = {

    val locusTag = feature.getQualifiers.get("locus_tag").get(0).getValue

    def getGeneName: String = {
      val tryGetGeneName = Try(feature.getQualifiers.get("gene").get(0).getValue)
      val name = tryGetGeneName match {
        case Success(properName) => tryGetGeneName.get
        case Failure(except) => locusTag
      }
      name
    }

    val geneName = getGeneName
    val geneTermList = List(new Term(geneName))
    val gene = new Gene(
      name = geneName,
      terms = geneTermList,
      coordinates = getCoordinates(feature),
      organism = organism,
      ccp = ccp,
      source = genbankSourceValue,
      properties = Map("locus_tag" -> locusTag))
    gene
  }

  def makeGenePolypeptideSequence(feature: NucleotideFeature, orgCCPSeq: (Organism, Node with CCP, DNASequence)): (Gene, Sequence, Polypeptide) = {

    def makeTranslation(feature: NucleotideFeature): Sequence = {
      val tryGetTranslation = Try(new Sequence(feature.getQualifiers.get("translation").get(0).getValue))
      val sequenceToCheck = tryGetTranslation match {
        case Success(seq) => tryGetTranslation.get
        case Failure(except) =>
          val coordinates = feature.getLocations
          val dnaSeqForTranslation = orgCCPSeq._3.getSequenceAsString(
            coordinates.getStart.getPosition,
            coordinates.getEnd.getPosition,
            coordinates.getStrand)
          val translatedAminoAcidSeq = DNATools.toProtein(DNATools.createDNA(dnaSeqForTranslation)).toString
          new Sequence(sequence = translatedAminoAcidSeq)
      }
      val md5 = sequenceToCheck.getMD5
      if (sequenceCollector.contains(md5)) sequenceCollector(md5)
      else {
        sequenceCollector = sequenceCollector ++ Map(md5 -> sequenceToCheck)
        sequenceToCheck
      }
    }

    val sequence = makeTranslation(feature)
    val gene = makeGene(feature, orgCCPSeq._1, orgCCPSeq._2)
//    sequence =  new Sequence(feature.getQualifiers.get("translation").get(0).getValue),
//    gene not have a transalation
//    println(gene.getName)
    val listOfXrefs = makeListOfXrefs(feature)

    val polypeptide = new Polypeptide(
      name = gene.getName,
      xRefs = listOfXrefs,
      sequence =  sequence,
      terms = gene.getTerms,
      gene = gene,
      organism = orgCCPSeq._1
    )
    (gene, sequence, polypeptide)
  }

  def makeGeneAndRNA(feature: NucleotideFeature, orgAndCCP: (Organism, Node with CCP, DNASequence), rnaType: String): (Gene, RNA) = {
    val properRNAType = rnaType match {
      case "ncRNA" => "sRNA"
      case _ => rnaType
    }
    val gene = makeGene(feature, orgAndCCP._1, orgAndCCP._2)
    val rna = new RNA(
      name = gene.getName,
      gene = gene,
      organism = orgAndCCP._1,
      rnaType = rnaType,
      xRefs = makeListOfXrefs(feature),
      source = genbankSourceValue
    )
    (gene, rna)
  }

  def makeSourceNodes(feature: NucleotideFeature, orgAndCCP: (Organism, Node with CCP, DNASequence)): CCP = {
    val organism = new Organism(
      name = feature.getQualifiers.get("organism").get(0).getValue,
      source = genbankSourceValue)
    val ccp = orgAndCCP._2.getType match {
      case CCPType.Chromosome => new Chromosome(
        name = orgAndCCP._2.getName,
        organism = organism,
        source = genbankSourceValue,
        length = orgAndCCP._2.getLength)
      case CCPType.Contig => new Contig(
        name = orgAndCCP._2.getName,
        organism = organism,
        source = genbankSourceValue,
        length = orgAndCCP._2.getLength)
      case CCPType.Plasmid => new Plasmid(
        name = orgAndCCP._2.getName,
        organism = organism,
        source = genbankSourceValue,
        length = orgAndCCP._2.getLength
      )
    }
    ccp
  }

  private def getCoordinates(feature: NucleotideFeature): Coordinates = {
    val location = feature.getLocations
    val strand = location.getStrand.getStringRepresentation match {
      case "+" => Strand.forward
      case "-" => Strand.reverse
      case _ => Strand.unknown
    }
//    new Coordinates(location.getStart.getPosition + 1, location.getEnd.getPosition, strand)
    new Coordinates(location.getStart.getPosition, location.getEnd.getPosition, strand)
  }

  private def makeListOfXrefs(feature: NucleotideFeature): List[XRef] = {

    def makeXref(ref: String): XRef = {
      val dbName = ref.split(":")(0)
      val xrefText = ref.split(":")(1)
      if (externalDataBasesCollector.contains(dbName)) new XRef(xrefText, externalDataBasesCollector(dbName))
      else {
        val newDB = new DBNode(dbName)
        val xrefNode = new XRef(xrefText, newDB)
        externalDataBasesCollector = externalDataBasesCollector ++ Map(dbName -> newDB)
        xrefNode
      }
    }
    try {

      val xrefs = feature.getQualifiers.get("db_xref")
      val resultXrefs = xrefs.asScala.toList.map(_.toString).filter(!_.contains("GeneID"))
      resultXrefs.map(ref => makeXref(ref))
    }
    catch {
      case except: NullPointerException =>
        logger.warn(feature.getDescription + " has no xrefs.")
        List()
    }
//    resultXrefs.map(ref=> new XRef(ref.split(":")(1), new DBNode(ref.split(":")(0))))

  }

//  def readOneSequence(seqFeatures: List[NucleotideFeature], seqOrganismAndCCP: (Organism, Node with CCP)): Unit = {
//    def processCurrentOrg = processFeature(_)(seqOrganismAndCCP)
//    seqFeatures.map(processCurrentOrg)
//  }


  def getUniqueFeatures(features: List[NucleotideFeature]) = {
    features.map(_.getType).toSet
  }

}
