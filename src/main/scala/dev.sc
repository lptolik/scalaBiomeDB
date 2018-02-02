//val polysMap = utilFunctionsObject.getPolypeptides("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
import java.io.File
import java.security.MessageDigest
import BioGraph._
import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions._
import sun.security.provider.MD5
import scala.collection.immutable.{HashMap, HashSet}
val genbank = DBNode("GenBank")
//val link = new linkTo(xref, uniprot)()
//val gene = new Gene(Map("name" -> "sad", "start" -> "123", "end" -> "150", "strand" -> "reverse"))
//link.endNode
//List(link.startNode, link.endNode)
val sl = List("some", "words").toString()
val str = "some"
sl == "List(some, words)"
//val strand = new Strand
val geneTerm2 = Term("Such standard")
val geneTerm = Term("Gene standard name")
val promoterTerm = Term("Gene standard name")
val organism = Organism("Eschrichia coli", List("GenBank"))
val uniprot = DBNode("UniProt", Map("some property" -> "U123"))
val xref = XRef("NZ_ACKO02000005", uniprot, Map("db_id" -> "NZ_ACKO02000005_GenBank"))
val xref2 = XRef("NZ_ACKO99999999", uniprot, Map("db_id" -> "NZ_ACKO99999999_GenBank"))
val chromosome = Chromosome("Corynebacterium glutamicum ATCC 13032, complete genome.", List("GenBank"), DNAType.circular, organism, 3282708, Map("some property" -> "works fine"))
val plasmid = Plasmid("Listeria grayi DSM 20601", organism = organism)
val contig = Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld1, whole genome shotgun sequence.", organism = organism)
//    val gene = Gene("gene name", Map())
val coordinates = Coordinates(150, 301, Strand.reverse)
val coordinates2 = Coordinates(387, 780, Strand.forward)
val coordinates3 = Coordinates(750, 1013, Strand.reverse)
val seq1 = SequenceDNA("MYR")
val seq2 = SequenceDNA("MRY")
val seq3 = SequenceDNA("KHR")
val gene = Gene("Super name", coordinates, plasmid, List(geneTerm), organism, List("GenBank", "MetaCyc"), seq1, Map("source" -> "GenBank"), nodeId = 134)
val gene2 = Gene("Such name", coordinates2, contig, List(geneTerm2), organism, List("GenBank"), seq2)
val gene3 = Gene("Super name", coordinates3, plasmid, List(geneTerm), organism, List("GenBank"), seq3, Map("source" -> "GenBank"), nodeId = 134)
gene.setProperties(Map("comment" -> "comment"))
gene.getCoordinates.getStrand
gene.getId
val promoter = Promoter("ydjFp1", Coordinates(1854924, 1854924, Strand.unknown), plasmid, organism, 1852948, promoterTerm, List("GenBank"))
promoter.getLabels
val aaa = "AR" -> "banana"
Coordinates(11, 12, Strand.forward)
class Properties[K, V](k: K, v: V){}
val props = new Properties[String, Double]("key", 1.23)
//val gene = gene.copy(coordinates = coordinates2, ccp = contig)
gene equals gene2
xref.equals(xref2.copy(xrefId = "NZ_ACKO02000005"))
//check BioEntity ==-opertaror (name, organism)
val miscFeature = MiscFeature("Misc_feature", Coordinates(1560, 6301, Strand.reverse), plasmid, List("GenBank"))
val miscStructure = MiscFeature("Misc_structure", Coordinates(3020, 3101, Strand.forward), plasmid, List("GenBank"))
//val operon = Operon("Very operonious", )
val boundaries = Boundaries(gene, gene3)
val operonTerm = Term("So much very operonious term")
val operon = Operon("So operonious", boundaries, operonTerm, organism)
val tuTerm1 = Term("So much very transcriptional unit")
val tuTerm2 = Term("Much more transcriptional unit")
val tu1 = TU("Very transcriptonal", tuTerm1, operon, promoter, organism, List(gene))
val tu2 = TU("More transcriptonal", tuTerm2, operon, promoter, organism, List(gene3))
operon.addTU(tu1)
operon.addTU(tu2)
//operon.tuList :: List(tu1)
operon.getTUs
val taxon1 = Taxon("Test taxon 1", TaxonType.phylum)
organism.setTaxon(taxon1)
//BLAST architecture//
val organismBlast = Organism("BLAST organism", List("GenBank"))
val DBBlast = DBNode("UniProt")
val sequenceBlast1 = SequenceAA("MSIQLNGINCFYGAHQALFDITLDCPQGETLVLLGPSGAGKSSLLRVLNLLEMPRSGTLNIAGNHFDFTKTPSDKAIRDLRRNVGMVFQQYNLWPHLTVQQNLIEAPCRVLGLSKDQALARAEKLLERLRLKPYSDRYPLHLSGGQQQRVAIARALMMEPQVLLFDEPTAALDPEITAQIVSIIRELAETNITQVIVTHEVEVARKTASRVVYMENGHIVEQGDASCFTEPQTEAFKNYLSH")
val sequenceBlast2 = SequenceAA("MNITATVLLAFGMSMDAFAASIGKGATLHKPKFSEALRTGLIFGAVETLTPLIGWGMGMLASRFVLEWNHWIAFVLLIFLGGRMIIEGFRGADDEDEEPRRRHGFWLLVTTAIATSLDAMAVGVGLAFLQVNIIATALAIGCATLIMSTLGMMVGRFIGSIIGKKAEILGGLVLIGIGVQILWTHFHG")
val sequenceBlast3 = SequenceAA("MPKIVILPHQDLCPDGAVLEANSGETILDAALRNGIEIEHACEKSCACTTCHCIVREGFDSLPESSEQEDDMLDKAWGLEPESRLSCQARVTDEDLVVEIPRYTINHAREH")
val xrefBlast1 = XRef("EG11568", uniprot)
val xrefBlast2 = XRef("EG10995", uniprot)
val xrefBlast3 = XRef("EG11850", uniprot)
val termBlast1 = Term("L-arginine ABC transporter")
val termBlast2 = Term("putative Mn(2+) efflux pump, mntR-regulated")
val termBlast3 = Term("reduced ferredoxin")
//val polyBlast1 = Polypeptide("Blast poly 1", List(xrefBlast1), sequenceBlast1, List(termBlast1), organismBlast)
//val polyBlast2 = Polypeptide("Blast poly 2", List(xrefBlast2), sequenceBlast2, List(termBlast2), organismBlast)
//val polyBlast3 = Polypeptide("Blast poly 3", List(xrefBlast3), sequenceBlast3, List(termBlast3), organismBlast)
val seqSet: Set[SequenceAA] = Set(sequenceBlast1, sequenceBlast2)
sequenceBlast1.similarities
sequenceBlast3.similarities
seqSet.filter(x => x equals sequenceBlast1).head.addSimilarity(Similarity(sequenceBlast3, 1e-10, 98.0 ))
seqSet.filter(x => x equals sequenceBlast1).head.getSimilarities
val sequenceBlast4 = sequenceBlast1.copy()
val setSeq = HashSet(sequenceBlast1)
tu1.consistsOf
utilFunctionsObject.getFromEntrez()
//polys.next().getProperty("seq")
//val polyMap = utilFunctionsObject.getPolypeptides("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
//val filtered = polyMap.filter(x => x._2.length > 1)
//filtered.size
//val gb = new GenBankUtil("/home/artem/work/reps/GenBank/e_coli_k_12.gb")
//val accessions = gb.getAccessionsFromGenBankFile
//val features = gb.getFeatures(accessions("NC_000913"))

//val l = gb.getInitialData(accessions("NC_000913"))
//l._3.getLength
//val setOfFeatures = accessions.values.map(gb.getFeatures).iterator//.foreach(x => println(x.size))
//val setOfOrganisms = accessions.values.map(gb.getInitialData).iterator//foreach(println)
//val zp = setOfFeatures zip setOfOrganisms
//zp.next()._1.length
//features(0).getQualifiers.get("organism").get(0).getValue

//features(516)
//val rec = accessions("NC_000913")
//rec.getSequenceAsString(58474, 59279, features(516).getLocations.getStrand)


//utilFunctionsObject.readInsideBlastResultFile("/home/artem/work/reps/GenBank/biome_api/biome/load/genbank/cross_blast_scala_text.txt").size
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

