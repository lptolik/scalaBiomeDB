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

/**
  * Created by artem on 25.04.16.
  */
class GenBankUtil(gbFileName: String) {
//  val geneList = List[Gene]()
  type NucletideFeature = FeatureInterface[AbstractSequence[_root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound], _root_.org.biojava.nbio.core.sequence.compound.NucleotideCompound]

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

  def getFeatures(dnaSeq: DNASequence): List[NucletideFeature] = {
    val features = dnaSeq.getFeatures.asScala.toList
    features
  }

  def processFeature(feature: NucletideFeature,
                     initialData: (Organism, Node with CCP)) = feature.getType match {
//    case source => ???
//      Set(Set(misc_feature, repeat_region, source, rRNA, mobile_element, ncRNA, tRNA, tmRNA, CDS, gene, rep_origin, STS))
//    case "gene" => makeRNA(feature)
//    case "source" => makeCCPandOrganism(feature)
    case "CDS" => makeGene(feature, initialData)
    case "tRNA" => makeRNA(feature)
    case "rRNA" => makeRNA(feature)
    case "ncRNA" => makeRNA(feature)
    case "tmRNA" => makeRNA(feature)
    case "mobile_element" => makeMiscFeature(feature)
    case "rep_origin" => makeMiscFeature(feature)
    case "STS" => makeMiscFeature(feature)
    case "misc_feature" => makeMiscFeature(feature)
    case "repeat_region" => makeMiscFeature(feature)
    case _ => println(feature.getType)
  }

  def makeCCPandOrganism(miscFeature: NucletideFeature) = {

  }

  def makeMiscFeature(miscFeature: NucletideFeature) = {

  }

  def makeGene(feature: NucletideFeature,
               initialData: (Organism, Node with CCP)): Gene = {

    val locusTag = feature.getQualifiers.get("locus_tag").get(0).getValue
    val geneName = feature.getQualifiers.get("gene").get(0).getValue

    val gene = new Gene(
      name = geneName,
      terms = List(new Term(geneName)),
      coordinates = getCoordinates(feature),
      organism = initialData._1,
      ccp = initialData._2,
      properties = Map("locus_tag" -> locusTag))
    gene
  }

  private def getCoordinates(feature: NucletideFeature): Coordinates = {
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

  private def makeListOfXrefs(feature: NucletideFeature): List[XRef] = {
    val xrefs = feature.getQualifiers.get("db_xref")
    val resultXrefs = xrefs.asScala.toList.map(ref => (ref.getName, ref.getValue)).filter(_._1 != "GeneID")
    resultXrefs.map(ref => new XRef(ref._2, new DBNode(ref._1)))
  }


  def makeRNA(miscFeature: NucletideFeature) = {}

  def getUniqueFeatures(features: List[NucletideFeature]) = {
    features.map(_.getType).toSet
  }

}
