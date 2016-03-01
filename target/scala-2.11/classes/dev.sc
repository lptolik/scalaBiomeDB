import java.security.MessageDigest

import BioGraph._
import utilFunctions._
import sun.security.provider.MD5

import scala.collection.immutable.HashSet


val genbank = new DBNode("GenBank")

//val link = new linkTo(xref, uniprot)()
//val gene = new Gene(Map("name" -> "sad", "start" -> "123", "end" -> "150", "strand" -> "reverse"))

//link.endNode
//List(link.startNode, link.endNode)
val sl = List("some", "words").toString()
val str = "some"
sl == "List(some, words)"
//val strand = new Strand
val geneTerm2 = new Term("Such standard")
val geneTerm = new Term("Gene standard name")
val promoterTerm = new Term("Gene standard name")
val organism = new Organism("Eschrichia coli")
val uniprot = new DBNode("UniProt", Map("some property" -> "U123"))
val xref = new XRef("NZ_ACKO02000005", uniprot, Map("db_id" -> "NZ_ACKO02000005_GenBank"))
val xref2 = new XRef("NZ_ACKO99999999", uniprot, Map("db_id" -> "NZ_ACKO99999999_GenBank"))
val chromosome = new Chromosome("Corynebacterium glutamicum ATCC 13032, complete genome.", SourceType.MetaCyc, DNAType.circular, organism, 3282708, Map("some property" -> "works fine"))
val plasmid = new Plasmid("Listeria grayi DSM 20601", organism = organism)
val contig = new Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld1, whole genome shotgun sequence.", organism = organism)
//    val gene = new Gene("gene name", Map())
val coordinates = new Coordinates(150, 301, Strand.reverse)
val coordinates2 = new Coordinates(387, 780, Strand.forward)
val coordinates3 = new Coordinates(750, 1013, Strand.reverse)
val gene = new Gene("Super name", coordinates, plasmid, geneTerm, organism, Map("source" -> "GenBank"), nodeId = 134)
val gene2 = new Gene("Such name", coordinates2, contig, geneTerm2, organism)
val gene3 = new Gene("Super name", coordinates3, plasmid, geneTerm, organism, Map("source" -> "GenBank"), nodeId = 134)
gene.setProperties(Map("comment" -> "new comment"))
gene.getCoordinates.getStrand
gene.getId
val promoter = new Promoter("ydjFp1", new Coordinates(1854924, 1854924, Strand.unknown), plasmid, organism, 1852948, promoterTerm)
promoter.getLabels
val aaa = ("AR" -> "banana")
Coordinates(11, 12, Strand.forward)
class Properties[K, V](k: K, v: V){}
val props = new Properties[String, Double]("key", 1.23)
val geneNew = gene.copy(coordinates = coordinates2, ccp = contig)
geneNew equals gene2

xref.equals(xref2.copy(xrefId = "NZ_ACKO02000005"))
//check BioEntity ==-opertaror (name, organism)
val miscFeature = new MiscFeature(new Coordinates(1560, 6301, Strand.reverse), plasmid)
val miscStructure = new MiscStructure(new Coordinates(3020, 3101, Strand.forward), plasmid)
//val operon = new Operon("Very operonious", )

val boundaries = new Boundaries(gene, gene3)
val operonTerm = new Term("So much very operonious term")
val operon = new Operon("So operonious", boundaries, operonTerm, organism)
val tuTerm1 = new Term("So much very transcriptional unit")
val tuTerm2 = new Term("Much more transcriptional unit")
val tu1 = new TU("Very transcriptonal", tuTerm1, operon, organism, List(gene))
val tu2 = new TU("More transcriptonal", tuTerm2, operon, organism, List(gene3))
operon.addTU(tu1)
operon.addTU(tu2)
//operon.tuList :: List(tu1)
operon.getTUs
val taxon1 = new Taxon("Test taxon 1", TaxonType.phylum)
organism.setTaxon(taxon1)

//BLAST architecture//
val organismBlast = new Organism("BLAST organism")
val DBBlast = new DBNode("UniProt")
val sequenceBlast1 = new Sequence("MSIQLNGINCFYGAHQALFDITLDCPQGETLVLLGPSGAGKSSLLRVLNLLEMPRSGTLNIAGNHFDFTKTPSDKAIRDLRRNVGMVFQQYNLWPHLTVQQNLIEAPCRVLGLSKDQALARAEKLLERLRLKPYSDRYPLHLSGGQQQRVAIARALMMEPQVLLFDEPTAALDPEITAQIVSIIRELAETNITQVIVTHEVEVARKTASRVVYMENGHIVEQGDASCFTEPQTEAFKNYLSH")
val sequenceBlast2 = new Sequence("MNITATVLLAFGMSMDAFAASIGKGATLHKPKFSEALRTGLIFGAVETLTPLIGWGMGMLASRFVLEWNHWIAFVLLIFLGGRMIIEGFRGADDEDEEPRRRHGFWLLVTTAIATSLDAMAVGVGLAFLQVNIIATALAIGCATLIMSTLGMMVGRFIGSIIGKKAEILGGLVLIGIGVQILWTHFHG")
val sequenceBlast3 = new Sequence("MPKIVILPHQDLCPDGAVLEANSGETILDAALRNGIEIEHACEKSCACTTCHCIVREGFDSLPESSEQEDDMLDKAWGLEPESRLSCQARVTDEDLVVEIPRYTINHAREH")
val xrefBlast1 = new XRef("EG11568", uniprot)
val xrefBlast2 = new XRef("EG10995", uniprot)
val xrefBlast3 = new XRef("EG11850", uniprot)
val termBlast1 = new Term("L-arginine ABC transporter")
val termBlast2 = new Term("putative Mn(2+) efflux pump, mntR-regulated")
val termBlast3 = new Term("reduced ferredoxin")
val polyBlast1 = new Polypeptide("Blast poly 1", xrefBlast1, sequenceBlast1, termBlast1, organismBlast)
val polyBlast2 = new Polypeptide("Blast poly 2", xrefBlast2, sequenceBlast2, termBlast2, organismBlast)
val polyBlast3 = new Polypeptide("Blast poly 3", xrefBlast3, sequenceBlast3, termBlast3, organismBlast)
val seqSet: Set[Sequence] = Set(sequenceBlast1, sequenceBlast2)
sequenceBlast1.similarities
sequenceBlast3.similarities
seqSet.filter(x => x equals sequenceBlast1).head.addSimilarity(sequenceBlast3)
seqSet.filter(x => x equals sequenceBlast1).head.similarities
val blastedSequences = utilFunctions.readInsideBlastResultFile("/home/artem/work/reps/GenBank/biome_api/biome/load/genbank/cross_blast_scala_text.txt").size



//sequenceBlast2.similarities
/////////////////////
//gene.getName
//gene.isInstanceOf[Int]
//val promoter = new Promoter(Map("source" -> "MetaCyc",
//  "name" -> "ydjFp1",
//  "end" -> 1854924,
//  "start" -> 1854924,
//  "strand" -> "unknown",
//  "tss" -> 1852948))
//promoter.getCoordinates

