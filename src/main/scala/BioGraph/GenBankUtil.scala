package BioGraph

import java.io.File
import java.util
import scala.collection.JavaConverters._
import org.biojava.nbio.core.sequence.DNASequence
import org.biojava.nbio.core.sequence.io.GenbankReaderHelper
import org.biojava.nbio.core.sequence.features.FeatureInterface
import org.biojava.nbio.core.sequence.template.AbstractSequence
import org.apache.logging.log4j.LogManager
import org.biojava.nbio.core.sequence.features.Qualifier

import scala.collection.immutable.{HashMap, Map}
import scala.util.{Failure, Success, Try}

/**
  * Created by artem on 25.04.16.
  */
class GenBankUtil(gbFileName: String) {
//  val geneList = List[Gene]()
  type NucleotideFeature = FeatureInterface[AbstractSequence[_root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound], _root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound]

  val logger = LogManager.getLogger(this.getClass.getName)

  def getAccessionsFromGenBankFile: Map[String, DNASequence] = {
    val gbFile = new File(gbFileName)
    val dnaSequences = GenbankReaderHelper.readGenbankDNASequence(gbFile)
    val accessions = HashMap() ++ dnaSequences.asScala
    accessions
  }

  def getInitialData(dnaSeq: DNASequence): (Organism, Node with CCP) = {
    val accession = dnaSeq.getAccession.toString
    val dnaLength = dnaSeq.getLength.toString
    val circular = if (dnaSeq.getOriginalHeader.contains("circular")) "circular" else "linear"
    val description = dnaSeq.getDescription.toUpperCase
    println(description)
    val ccpType =
      if (description.contains("COMPLETE GENOME") || description.contains("COMPLETE SEQUENCE")) "Chromosome"
      else if (description.contains("CONTIG")) "Contig"
      else if (description.contains("PLASMID")) "Plasmid"
      else {
        logger.warn("Unknown genome element")
        "Contig"
      }
    val organismName = dnaSeq.getFeatures(1).get(0).getQualifiers.get("organism").get(0).getValue
    val organism = new Organism(name = organismName, source = "GenBank")
    val ccp = ccpType match{
      case "Chromosome" => new Chromosome(name = description, organism = organism)
      case "Contig" => new Contig(name = description, organism = organism)
      case "Plasmid" => new Plasmid(name = description, organism = organism)
    }
//    List(accession, dnaLength, circular, ccpType, organismName)
    (organism, ccp)
  }

  def getFeatures(dnaSeq: DNASequence): List[NucleotideFeature] = {
    val features = dnaSeq.getFeatures.asScala.toList
    features
  }

  def processFeature(feature: NucleotideFeature,
                     orgAndCCP: (Organism, Node with CCP)) = feature.getType match {
//    case source => ???
//      Set(Set(misc_feature, repeat_region, source, rRNA, mobile_element, ncRNA, tRNA, tmRNA, CDS, gene, rep_origin, STS))
//    case "gene" => makeRNA(feature)
//    case "source" => makeCCPandOrganism(feature)
    case "CDS" => makeGeneAndPolypeptide(feature, orgAndCCP)
    case "tRNA" => makeGeneAndRNA(feature, orgAndCCP, feature.getType)
    case "rRNA" => makeGeneAndRNA(feature, orgAndCCP, feature.getType)
    case "ncRNA" => makeGeneAndRNA(feature, orgAndCCP, feature.getType)
    case "tmRNA" => makeGeneAndRNA(feature, orgAndCCP, feature.getType)
    case "mobile_element" => makeMiscFeature(feature, feature.getType, orgAndCCP: (Organism, Node with CCP))
    case "rep_origin" => makeMiscFeature(feature, feature.getType, orgAndCCP: (Organism, Node with CCP))
    case "STS" => makeMiscFeature(feature, feature.getType, orgAndCCP: (Organism, Node with CCP))
    case "misc_feature" => makeMiscFeature(feature, feature.getType, orgAndCCP: (Organism, Node with CCP))
    case "repeat_region" => makeMiscFeature(feature, feature.getType, orgAndCCP: (Organism, Node with CCP))
    case _ => logger.warn("Unknown feature type:" + feature.getType, orgAndCCP: (Organism, Node with CCP))
  }

  def makeCCPandOrganism(miscFeature: NucleotideFeature) = {

  }

  def makeMiscFeature(
                       miscFeature: NucleotideFeature,
                       miscFeatureType: String,
                       orgAndCCP: (Organism, Node with CCP)): Feature with DNA = {
    def getMiscFeatureAdditionalProperties: Try[String] = {
      Try(miscFeature.getQualifiers.get("note").get(0).getValue)
    }
    val note = getMiscFeatureAdditionalProperties
    val properties = note match {
      case Success(String) => Map[String, Any]("comment" -> miscFeature.getQualifiers.get("note").get(0).getValue)
      case Failure(Exception) => Map[String, Any]()
    }

    new MiscFeature(
      miscFeatureType = miscFeatureType,
      coordinates = getCoordinates(miscFeature),
      ccp = orgAndCCP._2,
      properties = properties
    )
  }
//    def makeMiscFeature(miscFeature: NucleotideFeature) = {}


  private def makeGene(feature: NucleotideFeature, organism: Organism, ccp: CCP): Gene = {

    val locusTag = feature.getQualifiers.get("locus_tag").get(0).getValue
    val geneName = feature.getQualifiers.get("gene").get(0).getValue
    val geneTermList = List(new Term(geneName))
    val gene = new Gene(
      name = geneName,
      terms = geneTermList,
      coordinates = getCoordinates(feature),
      organism = organism,
      ccp = ccp,
      properties = Map("locus_tag" -> locusTag))
    gene
  }

  def makeGeneAndPolypeptide(feature: NucleotideFeature, orgAndCCP: (Organism, Node with CCP)): (Polypeptide, Gene) = {
    val gene = makeGene(feature, orgAndCCP._1, orgAndCCP._2)
    val polypeptide = new Polypeptide(
      name = gene.getName,
      xRefs = makeListOfXrefs(feature),
      sequence =  new Sequence(feature.getQualifiers.get("translation").get(0).getValue),
      terms = gene.getTerms,
      gene = gene,
      organism = orgAndCCP._1
    )
    (polypeptide, gene)
  }

  def makeGeneAndRNA(feature: NucleotideFeature, orgAndCCP: (Organism, Node with CCP), rnaType: String): (RNA, Gene) = {
    val gene = makeGene(feature, orgAndCCP._1, orgAndCCP._2)
    val rna = new RNA(
      name = gene.getName,
      gene = gene,
      orgAndCCP._1,
      rnaType
    )
    (rna, gene)
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

//  private def makePolypeptide(feature: NucletideFeature): Polypeptide ={
//    new Polypeptide()
//  }

  private def makeListOfXrefs(feature: NucleotideFeature): List[XRef] = {
    val xrefs = feature.getQualifiers.get("db_xref")
    val resultXrefs = xrefs.asScala.toList.map(ref => (ref.getName, ref.getValue)).filter(_._1 != "GeneID")
    resultXrefs.map(ref => new XRef(ref._2, new DBNode(ref._1)))
  }

  private def getProperRNAType(rnaType: String): String = {
    "To be implemented"
  }


  def getUniqueFeatures(features: List[NucleotideFeature]) = {
    features.map(_.getType).toSet
  }

}
