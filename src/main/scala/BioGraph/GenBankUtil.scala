package BioGraph

import java.io.{File, PrintWriter}
import java.util

import org.biojava.nbio.core.exceptions.ParserException
import org.biojava.nbio.core.sequence.compound.{AmbiguityDNACompoundSet, NucleotideCompound}
import utilFunctions.TransactionSupport

import scala.collection.JavaConverters._
import org.biojava.nbio.core.sequence.DNASequence
import org.biojava.nbio.core.sequence.io.{DNASequenceCreator, GenbankReader, GenericGenbankHeaderParser}
import org.biojava.nbio.core.sequence.features.{FeatureInterface, Qualifier}
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

  type NucleotideFeature = FeatureInterface[AbstractSequence[_root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound], _root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound]

  val genbankSourceValue = List("GenBank")
  val logger = LogManager.getLogger(this.getClass.getName)
  logger.info("Start processing " + gbFile.getName)
  var sequenceAACollector: Map[String, SequenceAA] = Map()
  var sequenceDNACollector: Map[String, SequenceDNA] = Map()
  var termCollector: Map[String, Term] = Map()
  var externalDataBasesCollector: Map[String, DBNode] = Map()
  var xrefCollector: Map[String, XRef] = Map()

  private def correctDBXrefInFile(gbFile: File): File = {
    val outputFileName = gbFile.getAbsolutePath.split(".gb")(0) + "_corrected_dbxrefs.gb"
    val outputFile = new PrintWriter(new File(outputFileName))
    def rewriteWithProperXRefs(line: String): Unit = {
      if (line.contains("/db_xref=") && line.split(":")(1).contains(" ")) {
        val parts = line.split(":")
        val partOne = parts(0) + ":"
        val partTwo = parts(1).split(" ")(0) + "\"\n"
        outputFile.write(partOne + partTwo)
      }
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
        val correctedGbr = new GenbankReader[DNASequence, NucleotideCompound](
          correctDBXrefInFile(gbFile),
          new GenericGenbankHeaderParser[DNASequence, NucleotideCompound](),
          new DNASequenceCreator(AmbiguityDNACompoundSet.getDNACompoundSet)
        )
        val dnaSequences = correctedGbr.process()
        val accessions = HashMap() ++ dnaSequences.asScala
        accessions
    }
  }

  def getInitialData(dnaSeq: DNASequence): (Organism, Node with CCP, DNASequence) = {
    val accession = dnaSeq.getAccession.toString
    val dnaLength = dnaSeq.getLength.toString
    val circularOrLinear = if (dnaSeq.getOriginalHeader.contains("circular")) DNAType.circular else DNAType.linear
    val ccpLength = dnaSeq.getLength
    val descriptionForUpload = dnaSeq.getDescription
    val description = descriptionForUpload.toUpperCase
    val locus = dnaSeq.getOriginalHeader.split(" ").filter(_.nonEmpty)(0)
    val ccpType =
      if (description.contains("COMPLETE GENOME") || description.contains("COMPLETE SEQUENCE")) "Chromosome"
      else if (description.contains("CONTIG")) "Contig"
      else if (description.contains("PLASMID")) "Plasmid"
      else {
        logger.warn("Unknown CCP type")
        "Contig"
      }
//    val taxonID = dnaSeq
//      .getFeaturesByType("source")
//      .get(0)
//      .getQualifiers
//      .get("db_xref")
//      .get(0)
//      .toString
//      .split(":")(1)
//    features(0).getQualifiers.get("organism").get(0).getValue
    val organismName = dnaSeq
      .getFeaturesByType("source")
      .get(0)
      .getQualifiers
      .get("organism")
      .get(0)
      .getValue
//    val organismName = dnaSeq.getFeatures.get(0).getQualifiers.get("organism").get(0).getValue
    val organism = Organism(
      name = organismName,
      accessions = List(makeXref("GenBank:"  + accession)),
      taxon = Taxon("", TaxonType.no_rank),
      source = genbankSourceValue,
      properties = Map("accession" -> accession))

    val ccp = ccpType match{
      case "Chromosome" => Chromosome(
        name = locus,
        organism = organism,
        dnaType = circularOrLinear,
        source = genbankSourceValue,
        length = ccpLength,
        properties = Map("length" -> dnaLength))
      case "Contig" => Contig(
        name = locus,
        organism = organism,
        dnaType = circularOrLinear,
        source = genbankSourceValue,
        length = ccpLength,
        properties = Map("length" -> dnaLength))
      case "Plasmid" => Plasmid(
        name = locus,
        organism = organism,
        dnaType = circularOrLinear,
        source = genbankSourceValue,
        length = ccpLength,
        properties = Map("length" -> dnaLength))
    }
    (organism, ccp, dnaSeq)
  }

  def getFeatures(dnaSeq: DNASequence): List[NucleotideFeature] = {
    val features = dnaSeq.getFeatures.asScala.toList
    features
  }

  def processFeatures(orgAndCCP: (Organism, Node with CCP, DNASequence))(listOfFeatures: List[NucleotideFeature]): List[Any] = {
    listOfFeatures.map(processFeature(orgAndCCP: (Organism, Node with CCP, DNASequence))(_))
  }

  private def processFeature(orgAndCCP: (Organism, Node with CCP, DNASequence))(feature: NucleotideFeature) = feature.getType match {
    case "CDS" => makeGenePolypeptideSequence(feature, orgAndCCP)
    case "gene" => makePseudoGene(feature, orgAndCCP)
    case "tRNA" | "rRNA" | "ncRNA" | "tmRNA" => makeGeneAndRNA(feature, orgAndCCP, feature.getType)
    case "mobile_element" | "rep_origin" | "STS" | "misc_feature" | "repeat_region" =>
      makeMiscFeature(feature, feature.getType, orgAndCCP)
    case "source" => //makeSourceNodes(feature, orgAndCCP)
    case _ => logger.warn("Unknown feature type: " + feature.getType + " in file " + gbFile.getName)
  }

  def makeMiscFeature(
                       miscFeature: NucleotideFeature,
                       miscFeatureType: String,
                       orgAndCCP: (Organism, Node with CCP, DNASequence)): (Option[Feature with DNA]) = {
    def getMiscFeatureAdditionalProperties: Try[String] = {
      Try(miscFeature.getQualifiers.get("note").get(0).getValue)
    }
    val note = getMiscFeatureAdditionalProperties
    val properties = note match {
      case Success(properNote) => Map[String, Any]("comment" -> properNote)
      case Failure(except) => Map[String, Any]()
    }

    val misc = MiscFeature(
      miscFeatureType = miscFeatureType.capitalize,
      coordinates = makeCoordinates(miscFeature),
      ccp = orgAndCCP._2,
      source = genbankSourceValue,
      properties = properties
    )
    Some(misc)
  }

  def makePseudoGene(feature: NucleotideFeature, orgAndCCP: (Organism, Node with CCP, DNASequence)) = {
    if (feature.getQualifiers.containsKey("pseudo")) {
      val geneAndSeq = makeGene(feature, orgAndCCP._1, orgAndCCP._2, orgAndCCP._3)
      (Some(geneAndSeq._1), Some(geneAndSeq._2))
//      gene.copy(properties = Map("comment" -> "pseudo"))
    }
  }

  private def makeGene(feature: NucleotideFeature, organism: Organism, ccp: CCP, genome: DNASequence): (SequenceDNA, Gene) = {
    val tryGetLocusTag = Try(feature.getQualifiers.get("locus_tag").get(0).getValue)
    val locusTag = tryGetLocusTag match {
      case Success(properLocusTag) => tryGetLocusTag.get
      case Failure(except) =>
        val coord = makeCoordinates(feature)
        logger.warn("A gene without locus tag at "
          + coord.getStart + "-" + coord.getEnd
          + " in " + gbFile.getName)
        "hypothetical protein"
    }

    def getGeneNames(typeOfName: String): Option[util.List[Qualifier]] = {
      val tryGetGeneName = Option(feature.getQualifiers.get(typeOfName))
      tryGetGeneName
    }

    def getGeneName: String = {
      val tryGetGeneName = getGeneNames("gene") //Try(feature.getQualifiers.get("gene").get(0).getValue)
      val name = tryGetGeneName match {
        case Some(properName) => properName.get(0).getValue
        case None =>
          logger.warn("Gene " + feature.getLocations + " has no gene-name in " + gbFile.getName)
          locusTag
      }
      name
    }

    def getGeneSynonymNames: List[Term] = {
      val tryGetGeneName = getGeneNames("gene_synonym")
      val name = tryGetGeneName match {
        case Some(properName) => properName.get(0).getValue
        case None => ""
      }
      if (name.nonEmpty) {
        val synonyms = name.split(";").toList
        val synonymTerms = synonyms.map(s => Term(s))
        synonymTerms
      }
      else List()
    }

    def getECNumber: List[Term] = {
      val tryGetECNumber = getGeneNames("EC_number")
      val ecNumberTerm = tryGetECNumber match {
        case Some(properName) =>
          val ecNumber = properName.get(0).getValue
          List(Term(ecNumber))
        case None =>
          List()
      }
      ecNumberTerm
    }

    val geneName = getGeneName
    val geneTermList = getECNumber ++ List(Term(locusTag)) ++ List(Term(geneName)) ++ getGeneSynonymNames

    val DNASeq = getDNASequence(genome, feature)
    val geneSeq: SequenceDNA = utilFunctions.utilFunctionsObject.checkSequenceDNA(DNASeq) match {
      case true => SequenceDNA(sequence = getDNASequence(genome, feature), translatable = true)
      case false => SequenceDNA(sequence = getDNASequence(genome, feature), translatable = false)
    }

    val md5 = geneSeq.getMD5
    val outputSequence = sequenceDNACollector.contains(md5) match {
      case true => sequenceDNACollector(md5)
      case false =>
        sequenceDNACollector = sequenceDNACollector ++ Map(md5 -> geneSeq)
        geneSeq
    }

    val gene = Gene(
      name = geneName,
      terms = checkTermsForDuplicates(geneTermList),
      coordinates = makeCoordinates(feature),
      organism = organism,
      ccp = ccp,
      source = genbankSourceValue,
      sequence = outputSequence,
      properties = Map("locus_tag" -> locusTag))
    (outputSequence, gene)
  }

  def makeGenePolypeptideSequence(
                                   feature: NucleotideFeature,
                                   orgCCPSeq: (Organism, Node with CCP, DNASequence)):
  (Option[SequenceDNA], Option[Gene], Option[SequenceAA], Option[Polypeptide]) = {

    def makeTranslation(translatable: Boolean): Option[SequenceAA] = {
      val coordinates = getFeatureCoordinates(feature)
      val tryGetTranslation = Try(feature.getQualifiers.get("translation").get(0).getValue)
      val sequenceToCheck: String = tryGetTranslation match {
        case Success(seq) => tryGetTranslation.get.toUpperCase
        case Failure(except) =>
          if (translatable) {
            val dnaSeqForTranslation = getDNASequence(orgCCPSeq._3, feature)
            val translatedSequence = DNATools.toProtein(DNATools.createDNA(dnaSeqForTranslation)).seqString.toUpperCase
            if (!translatedSequence.contains('*')) translatedSequence
            else if (translatedSequence.groupBy(identity)('*').length == 1 && translatedSequence.endsWith("*")) {
              translatedSequence.replace("*", "")
            }
            else {
              logger.warn(s"DNA sequence was not translated properly. Coordinates: ${coordinates._1}, ${coordinates._2}, ${coordinates._3.toString}")
              ""
            }
          }
          else {
            logger.warn(s"Untranslatable gene sequence. Coordinates: ${coordinates._1}, ${coordinates._2}, ${coordinates._3.toString}")
            ""
          }
      }
      if (sequenceToCheck.nonEmpty) {
        val sequenceObject = SequenceAA(sequence = sequenceToCheck)
        val md5 = sequenceObject.getMD5
        val outputSequence = sequenceAACollector.contains(md5) match {
          case true => sequenceAACollector(md5)
          case false =>
            sequenceAACollector = sequenceAACollector ++ Map(md5 -> sequenceObject)
            sequenceObject
        }
        Option(outputSequence)
      }
      else None
    }

    val geneSeqAndGene = makeGene(feature, orgCCPSeq._1, orgCCPSeq._2, orgCCPSeq._3)
    val geneSeq = geneSeqAndGene._1
    val gene = geneSeqAndGene._2
    val sequence = makeTranslation(geneSeq.translatable)

    val polypeptide = sequence match {
      case Some(s) =>
        val proteinId = Option(feature.getQualifiers.get("protein_id"))
        val listOfXrefs = proteinId match {
          case Some(p) => makeListOfXrefs(feature) ++ List(makeXref("NCBI:" + p.get(0).getValue))
          case None => makeListOfXrefs(feature)
        }
        val product = getProduct(feature)
        val polypeptideTerms = getTermForGeneProduct(product, gene) ++ gene.getTerms

        Option(Polypeptide(
          name = gene.getName,
          xRefs = listOfXrefs,
          sequence = s,
          terms = checkTermsForDuplicates(polypeptideTerms.distinct),
          gene = gene,
          organism = orgCCPSeq._1,
          properties = product
          )
        )
      case None => None
    }
    (Some(geneSeq), Some(gene), sequence, polypeptide)
  }

  def makeGeneAndRNA(
                      feature: NucleotideFeature,
                      orgAndCCP: (Organism, Node with CCP, DNASequence),
                      rnaType: String):
  (Option[SequenceDNA], Option[Gene], Option[RNA]) = {
    val properRNAType = rnaType match {
      case "ncRNA" => "sRNA"
      case _ => rnaType
    }
    val geneSeqAndGene = makeGene(feature, orgAndCCP._1, orgAndCCP._2, orgAndCCP._3)
    val geneSeq = geneSeqAndGene._1
    val gene = geneSeqAndGene._2
    val rna = RNA(
      name = gene.getName,
      gene = gene,
      organism = orgAndCCP._1,
      rnaType = properRNAType,
      xRefs = makeListOfXrefs(feature),
      source = genbankSourceValue,
      properties = getProduct(feature)
    )
    (Some(geneSeq), Some(gene), Some(rna))
  }

  def makeSourceNodes(
                       feature: NucleotideFeature,
                       orgAndCCP: (Organism, Node with CCP, DNASequence)): Option[CCP] = {
    val organism = Organism(
      name = feature.getQualifiers.get("organism").get(0).getValue,
      source = genbankSourceValue)
    val ccp = orgAndCCP._2.getType match {
      case CCPType.Chromosome => Chromosome(
        name = orgAndCCP._2.getName,
        organism = organism,
        source = genbankSourceValue,
        length = orgAndCCP._2.getLength)
      case CCPType.Contig => Contig(
        name = orgAndCCP._2.getName,
        organism = organism,
        source = genbankSourceValue,
        length = orgAndCCP._2.getLength)
      case CCPType.Plasmid => Plasmid(
        name = orgAndCCP._2.getName,
        organism = organism,
        source = genbankSourceValue,
        length = orgAndCCP._2.getLength
      )
    }
    Some(ccp)
  }

  private def getFeatureCoordinates(feature: NucleotideFeature) = {
    val coordinates = feature.getLocations
    val start = coordinates.getStart.getPosition
    val end = coordinates.getEnd.getPosition
    val strand = coordinates.getStrand
    (start, end, strand)
  }

  private def getDNASequence(genome: DNASequence, feature: NucleotideFeature): String = {
    val coordinates = getFeatureCoordinates(feature)
    val dnaSeqForTranslation = genome
      .getSequenceAsString(
        coordinates._1,
        coordinates._2,
        coordinates._3)
      .toUpperCase()
    dnaSeqForTranslation
  }

  private def makeCoordinates(feature: NucleotideFeature): Coordinates = {
    val location = getFeatureCoordinates(feature)
    val strand = location._3.getStringRepresentation match {
      case "+" => Strand.forward
      case "-" => Strand.reverse
      case _ => Strand.unknown
    }
//    new Coordinates(location.getStart.getPosition + 1, location.getEnd.getPosition, strand)
    Coordinates(location._1, location._2, strand)
  }

  private def makeXref(ref: String): XRef = {
    val dbName = ref.split(":")(0)
    val xrefText = ref.split(":")(1)

    val dbNode = getOrCreateDBNode(dbName)
    if (xrefCollector.contains(xrefText)) xrefCollector(xrefText)
    else {
      val xref = XRef(xrefText, dbNode)
      xrefCollector ++= Map(xrefText -> xref)
      xref
    }
  }

  private def getOrCreateDBNode(dbName: String): DBNode = {
    if (externalDataBasesCollector.contains(dbName)) externalDataBasesCollector(dbName)
    else {
      val db = DBNode(dbName)
      externalDataBasesCollector = externalDataBasesCollector ++ Map(dbName -> db)
      db
    }
  }

  private def getProduct(feature: NucleotideFeature): Map[String, String] = {
    val tryGetProduct = Option(feature.getQualifiers.get("product"))
    val product = tryGetProduct match {
      case Some(p) => Map("product" -> p.get(0).getValue)
      case None =>
        val emptyMap: Map[String, String] = Map()
        emptyMap
    }
    product
  }

  private def makeListOfXrefs(feature: NucleotideFeature): List[XRef] = {
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

  }

  private def getTermForGeneProduct(product: Map[String, String], gene: Gene): List[Term] = {
    val termList = product.nonEmpty match {
      case true => gene.getTerms ++ List(Term(product("product")))
      case false => gene.getTerms
    }
    termList
  }

  private def checkTermsForDuplicates(listOfTerms: List[Term]): List[Term] = {
    def checker(term: Term): Term = {
      if (termCollector.contains(term.getText)) termCollector(term.getText)
      else {
        termCollector = termCollector ++ Map(term.getText -> term)
        term
      }
    }
    listOfTerms.distinct.map(checker)
  }

  def getUniqueFeatures(features: List[NucleotideFeature]) = {
    features.map(_.getType).toSet
  }

}
