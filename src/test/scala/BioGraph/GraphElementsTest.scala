package BioGraph

import org.scalatest.FunSuite
import utilFunctions.utilFunctionsObject

/**
  * Created by artem on 11.02.16.
  */
class GraphElementsTest extends FunSuite {

  trait TestNodesAndRels {
    val geneTerm = Term("Gene standard name")
    val geneTermCopy = Term("Gene standard name")
    val geneTerm2 = Term("Such standard")
    val geneTerm3 = Term("The most common gene name")
    val geneTerm4 = Term("The most common gene name on forward strand")
    val geneTerm5 = Term("The most common gene name in another organism")

    val promoterTerm = Term("Gene standard name")

    val taxon = Taxon("Very taxonious", TaxonType.family)
    val taxonCopy = Taxon("Very taxonious", TaxonType.family)
    val taxon2 = Taxon("Much more taxonious", TaxonType.genus)

    val organism = Organism("Eschrichia coli", taxon = taxon, source = List("GenBank"))
    val organismCopy = Organism("Eschrichia coli", List("GenBank"), taxon = taxon)
    val organism2 = Organism("Bacillus subtilis", List("GenBank"), taxon = taxon2)

    val uniprot = DBNode("UniProt", Map("some property" -> "U123"))
    val xref = XRef("NZ_ACKO02000005", uniprot, Map("db_id" -> "NZ_ACKO02000005_GenBank"))
    val xrefCopy = XRef("NZ_ACKO02000005", uniprot, Map("db_id" -> "NZ_ACKO02000005_GenBank"))
    val link = new LinkTo(xref, uniprot)

    val chromosome1 = Chromosome("Bacillus subtilis subsp. subtilis str. 168 complete genome.", List("MetaCyc"), DNAType.circular, organism, 4215606, Map("bacilus property" -> "bacilus is fine"))
    val chromosome1Copy = Chromosome("Bacillus subtilis subsp. subtilis str. 168 complete genome.", List("MetaCyc"), DNAType.circular, organism, 4215606, Map("bacilus property" -> "bacilus is fine"))
    val chromosome2 = Chromosome("Corynebacterium glutamicum ATCC 13032, complete genome.", List("MetaCyc"), DNAType.circular, organism, 3282708, Map("some property" -> "works fine"))
    val plasmid1 = Plasmid("Listeria grayi DSM 20601", organism = organism)
    val plasmid1Copy = Plasmid("Listeria grayi DSM 20601", organism = organism)
    val plasmid2 = Plasmid("Bacteroides fragilis 3_1_12 plasmid unnamed supercont", organism = organism)
    val contig1 = Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld1, whole genome shotgun sequence.", organism = organism)
    val contig1Copy = Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld1, whole genome shotgun sequence.", organism = organism)
    val contig2 = Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld0, whole genome shotgun sequence.", organism = organism)

    val coordinates1 = Coordinates(150, 301, Strand.reverse)
    val coordinates1Copy = Coordinates(150, 301, Strand.reverse)
    val coordinates2 = Coordinates(2460, 2874, Strand.reverse)
    val coordinates3 = Coordinates(750, 1013, Strand.reverse)
    val coordinates4 = Coordinates(1753, 1917, Strand.forward)
    val coordinates5 = Coordinates(524, 729, Strand.reverse)

    val gene1 = Gene("Super name", coordinates1, plasmid1, List(geneTerm), organism, source = List("GenBank"), SequenceDNA("ATGC"), Map("source" -> "GenBank"))
    val gene2 = Gene("Such name", coordinates2, plasmid1, List(geneTerm2), organism, source = List("GenBank"), SequenceDNA("ATGC"))
    val gene3 = Gene("Super long gene name", coordinates3, plasmid1, List(geneTerm3), organism, source = List("GenBank"), SequenceDNA("ATGC"), Map("source" -> "GenBank"))
    val gene4 = Gene("Super long gene name on forward strand", coordinates4, plasmid1, List(geneTerm4), organism, source = List("GenBank"), SequenceDNA("ATGC"), Map("source" -> "GenBank"))
    val gene5 = Gene("Super long gene name in another organism", coordinates4, contig1, List(geneTerm4), organism, source = List("GenBank"), SequenceDNA("ATGC"), Map("source" -> "GenBank"))

    val evidence = new Evidence(gene1, xref)

    val promoter = Promoter("ydjFp1", Coordinates(1854924, 1854924, Strand.unknown), plasmid1, organism, 1852948, promoterTerm, source = List("GenBank"))
    val terminator = Terminator(Coordinates(4633111, 4633144, Strand.forward), plasmid1,source = List("GenBank"))
    val miscFeature = MiscFeature("Misc_feature", Coordinates(1560, 6301, Strand.reverse), plasmid1, source = List("GenBank"))
    val miscStructure = MiscFeature("Misc_structure", Coordinates(3020, 3101, Strand.forward), plasmid1, source = List("GenBank"))
    val mobileElement = MobileElement("So mobile", Coordinates(3020, 3101, Strand.forward), plasmid1, source = List("GenBank"))

    val boundaries1 = Boundaries(gene1, gene2)
    val boundaries1Copy = Boundaries(gene1, gene2)
    val boundaries2 = Boundaries(gene3, gene2)

    val sequence1 = SequenceAA("MSIQLNGINCFYGAHQALFDITLDCPQGETLVLLGPSGAGKSSLLRVLNLLEMPRSGTLNIAGNHFDFTKTPSDKAIRDLRRNVGMVFQQYNLWPHLTVQQNLIEAPCRVLGLSKDQALARAEKLLERLRLKPYSDRYPLHLSGGQQQRVAIARALMMEPQVLLFDEPTAALDPEITAQIVSIIRELAETNITQVIVTHEVEVARKTASRVVYMENGHIVEQGDASCFTEPQTEAFKNYLSH")
    val sequence1Copy = SequenceAA("MSIQLNGINCFYGAHQALFDITLDCPQGETLVLLGPSGAGKSSLLRVLNLLEMPRSGTLNIAGNHFDFTKTPSDKAIRDLRRNVGMVFQQYNLWPHLTVQQNLIEAPCRVLGLSKDQALARAEKLLERLRLKPYSDRYPLHLSGGQQQRVAIARALMMEPQVLLFDEPTAALDPEITAQIVSIIRELAETNITQVIVTHEVEVARKTASRVVYMENGHIVEQGDASCFTEPQTEAFKNYLSH")
    val sequence2 = SequenceAA("MNITATVLLAFGMSMDAFAASIGKGATLHKPKFSEALRTGLIFGAVETLTPLIGWGMGMLASRFVLEWNHWIAFVLLIFLGGRMIIEGFRGADDEDEEPRRRHGFWLLVTTAIATSLDAMAVGVGLAFLQVNIIATALAIGCATLIMSTLGMMVGRFIGSIIGKKAEILGGLVLIGIGVQILWTHFHG")
    val sequence3 = SequenceAA("MPKIVILPHQDLCPDGAVLEANSGETILDAALRNGIEIEHACEKSCACTTCHCIVREGFDSLPESSEQEDDMLDKAWGLEPESRLSCQARVTDEDLVVEIPRYTINHAREH")

    val term1 = Term("L-arginine ABC transporter")
    val term2 = Term("putative Mn(2+) efflux pump, mntR-regulated")
    val term3 = Term("reduced ferredoxin")

    val xref1 = XRef("EG11568", uniprot)
    val xref2 = XRef("EG10995", uniprot)
    val xref3 = XRef("EG11850", uniprot)

    val poly1 = Polypeptide("Test poly 1", List(xref1), sequence1, List(term1), gene1, organism, List("GenBank", "MetaCyc"))
    val poly1Copy = poly1.copy()
    val poly2 = Polypeptide("Test poly 2", List(xref2), sequence2, List(term2), gene2, organism)
    val poly3 = Polypeptide("Test poly 3", List(xref3), sequence3, List(term3), gene3, organism)

    val tuTerm1 = Term("So much very transcriptional unit")
    val tuTerm2 = Term("Much more transcriptional unit")

    val operonTerm = Term("So much very operonious term")
    val operon = Operon("So operonious", boundaries1, operonTerm, organism)

    val tu1 = TU("Very transcriptonal", tuTerm1, operon, promoter, organism, List(gene1))
    val tu2 = TU("More transcriptonal", tuTerm2, operon, promoter, organism, List(gene3))
    operon.addTU(tu1)

    val compound1 = Compound(
      "oxybuprocaine",
      inchi = "InChI=1S/C17H28N2O3/c1-4-7-11-21-16-13-14(8-9-15(16)18)17(20)22-12-10-19(5-2)6-3/h8-9,13H,4-7,10-12,18H2,1-3H3",
      smiles = "CCCCCCCCCC(CCCCCCCCCCCC([O-])=O)OC1(OC(C(C(C1OC2(OC(C(C(C2O)O)O)COC(C)=O))O)O)COC(=O)C)",
      reference = List(xref1))
    val compound1Copy = Compound(
      "oxybuprocaine",
      inchi = "InChI=1S/C17H28N2O3/c1-4-7-11-21-16-13-14(8-9-15(16)18)17(20)22-12-10-19(5-2)6-3/h8-9,13H,4-7,10-12,18H2,1-3H3",
      smiles = "CCCCCCCCCC(CCCCCCCCCCCC([O-])=O)OC1(OC(C(C(C1OC2(OC(C(C(C2O)O)O)COC(C)=O))O)O)COC(=O)C)",
      reference = List(xref1))
    val compound2 = Compound(
      "\t2-styrylquinoline",
      inchi = "InChI=1S/C17H13N/c1-2-6-14(7-3-1)10-12-16-13-11-15-8-4-5-9-17(15)18-16/h1-13H")

    val similarity1 = Similarity(sequence2, 1e-12, 78.5)
    val similarity1Copy = Similarity(sequence2, 1e-12, 78.5)
    val similarity2 = Similarity(sequence1, 1e-15, 87.4)
    sequence3.addSimilarity(similarity1)

    val rna1 = RNA("5S ribosomal RNA", gene4, organism, "sRNA", List(xref1))
    val rna1Copy = rna1.copy()
    val rna2 = RNA("16S ribosomal RNA", gene5, organism, "sRNA", List(xref2))
    val rna3 = RNA("15S ribosomal RNA", gene4, organism2, "tRNA", List(xref3))

    val intactDBNode = DBNode("Intact")
    val reactant1 = BiochemicalReactant("Super reactant", sequence = "MHQPQWWHP", toCheck = true)
    val reactant1Copy = reactant1.copy()
    val reactant2 = BiochemicalReactant("Mega reactant")
    val reaction1 = BiochemicalReaction("Two reactant reaction", List(reactant1, reactant2))
    val reaction1Copy = reaction1.copy()
    val reaction2 = BiochemicalReaction("Another two reactant reaction", List(reactant1, reactant2))


  }

  test("test DBNode getLabels") {
    new TestNodesAndRels {
      assert(uniprot.getLabels === List("DB"))
    }
  }

  test("test getProperties DBNode") {
    new TestNodesAndRels {
      assert(uniprot.getProperties === Map("some property" -> "U123"))
    }
  }

  test("test getName DBNode") {
    new TestNodesAndRels {
      assert(uniprot.getName === "UniProt")
    }
  }

  test("test XRef getLabels  ") {
    new TestNodesAndRels {
      assert(xref.getLabels === List("XRef"))
    }
  }

  test("test XRef getProperties") {
    new TestNodesAndRels {
      assert(xref.getProperties === Map("db_id" -> "NZ_ACKO02000005_GenBank"))
    }
  }

  test("test XRef getXRef") {
    new TestNodesAndRels {
      assert(xref.getXRef === "NZ_ACKO02000005")
    }
  }

  test("test XRef getDB") {
    new TestNodesAndRels {
      assert(xref.getDB === uniprot)
    }
  }

  test("test XRef equals negative") {
    new TestNodesAndRels {
      assert((xref equals xref1) === false)
    }
  }

  test("test XRef equals positive") {
    new TestNodesAndRels {
      assert((xrefCopy equals xref) === true)
    }
  }

  test("get labels link test") {
    new TestNodesAndRels {
      assert(link.getLabel === "LINK_TO")
    }
  }

  test("start end nodes link test") {
    new TestNodesAndRels {
      assert(List(link.startNode, link.endNode) === List(xref, uniprot))
    }
  }

  test("test evidence getLabels") {
    new TestNodesAndRels {
      assert(evidence.getLabel === "EVIDENCE")
    }
  }

  test("get properties evidence test") {
    new TestNodesAndRels {
      assert(evidence.getProperties === Map())
    }
  }

  test("test evidence start end nodes  ") {
    new TestNodesAndRels {
      assert(List(evidence.startNode, evidence.endNode) === List(gene1, xref))
    }
  }

  test("test chromosome getProperties") {
    new TestNodesAndRels {
      assert(chromosome2.getProperties === Map("some property" -> "works fine"))
    }
  }

  test("test chromosome getLabels") {
    new TestNodesAndRels {
      assert(chromosome2.getLabels === List("Chromosome", "BioEntity"))
    }
  }

  test("test chromosome getLength") {
    new TestNodesAndRels {
      assert(chromosome2.getLength === 3282708)
    }
  }

  test("test chromosome getType") {
    new TestNodesAndRels {
      assert(chromosome2.getType === CCPType.Chromosome)
    }
  }

  test("test chromosome getSource") {
    new TestNodesAndRels {
      assert(chromosome2.getSource === List("MetaCyc"))
    }
  }

  test("test chromosome getName") {
    new TestNodesAndRels {
      assert(chromosome2.getName === "Corynebacterium glutamicum ATCC 13032, complete genome.")
    }
  }

  test("test chromosome equals negative") {
    new TestNodesAndRels {
      assert((chromosome2 equals chromosome1) === false)
    }
  }

  test("test chromosome equals positive") {
    new TestNodesAndRels {
      assert((chromosome1Copy equals chromosome1) === true)
    }
  }

  test("test plasmid getLabels") {
    new TestNodesAndRels {
      assert(plasmid1.getLabels === List("Plasmid", "BioEntity"))
    }
  }

  test("test plasmid equals negative") {
    new TestNodesAndRels {
      assert((plasmid1 equals plasmid2) === false)
    }
  }

  test("test plasmid equals positive") {
    new TestNodesAndRels {
      assert((plasmid1Copy equals plasmid1) === true)
    }
  }

  test("test contig getLabels") {
    new TestNodesAndRels {
      assert(contig1.getLabels === List("Contig", "BioEntity"))
    }
  }

  test("test contig equals negative") {
    new TestNodesAndRels {
      assert((contig1 equals contig2) === false)
    }
  }

  test("test contig equals positive") {
    new TestNodesAndRels {
      assert((contig1Copy equals contig1) === true)
    }
  }

  test("test gene getLabels") {
    new TestNodesAndRels {
      assert(gene1.getLabels === List("Gene", "BioEntity", "Feature", "DNA"))
    }
  }

  test("test gene getName") {
    new TestNodesAndRels {
      assert(gene1.getName === "Super name")
    }
  }

  test("test gene getCoordinates") {
    new TestNodesAndRels {
      assert(gene1.getCoordinates === Coordinates(150, 301, Strand.reverse))
    }
  }

  test("test gene getStandardName") {
    new TestNodesAndRels {
      assert(gene1.getTerms === List(geneTerm))
    }
  }

  test("test gene getProduct") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        gene1.getProduct
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test gene controlledBy") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        gene1.controlledBy
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test feature/gene previous") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        gene1.previous
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test feature/gene next") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        gene1.next
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test feature/gene overlaps") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        gene1.overlaps
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test promoter getLabels") {
    new TestNodesAndRels {
      assert(promoter.getLabels === List("Promoter", "BioEntity", "Feature", "DNA"))
    }
  }

  test("test promoter getName") {
    new TestNodesAndRels {
      assert(promoter.getName === "ydjFp1")
    }
  }

  test("test promoter getStandardName") {
    new TestNodesAndRels {
      assert(promoter.getStandardName === promoterTerm)
    }
  }

  test("test promoter getOrganism") {
    new TestNodesAndRels {
      assert(promoter.getOrganism === organism)
    }
  }

  test("test promoter getRegulationType") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        promoter.getRegulationType
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test terminator getLabels") {
    new TestNodesAndRels {
      assert(terminator.getLabels === List("Terminator", "Feature", "DNA"))
    }
  }

  test("get labels MiscFeature test") {
    new TestNodesAndRels {
      assert(miscFeature.getLabels === List("Misc_feature", "Feature", "DNA"))
    }
  }

  test("get labels MiscStructure test") {
    new TestNodesAndRels {
      assert(miscStructure.getLabels === List("Misc_structure", "Feature", "DNA"))
    }
  }

  test("get labels mobileElement test") {
    new TestNodesAndRels {
      assert(mobileElement.getLabels === List("Mobile_element", "Feature", "BioEntity", "DNA"))
    }
  }

  test("test mobileElement getName") {
    new TestNodesAndRels {
      assert(mobileElement.getName === "So mobile")
    }
  }

  /*
  test("test utilFunctionsObject.readInsideBlastResultFile result") {
    new TestNodesAndRels {
      //val blastedSequences: Map[Int, SequenceAA] = utilFunctionsObject.readInsideBlastResultFile("load/genbank/cross_blast_scala_text_upper.txt")
      //assert(blastedSequences.size === 242)
      assert(true)
    }
  }


   */
  test("test coordinates required start and end") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        Coordinates(101, 100, Strand.forward)
      }
      assert(thrown.getMessage === "requirement failed: Start coordinate cannot have bigger value than end coordinate!")
    }
  }

  test("test coordinates getStart") {
    new TestNodesAndRels {
      assert(coordinates1.getStart === 150)
    }
  }

  test("test coordinates getEnd") {
    new TestNodesAndRels {
      assert(coordinates1.getEnd === 301)
    }
  }

  test("test coordinates getStrand") {
    new TestNodesAndRels {
      assert(coordinates1.getStrand === Strand.reverse)
    }
  }

  test("test coordinates equals negative") {
    new TestNodesAndRels {
      assert((coordinates1 equals coordinates2) === false)
    }
  }

  test("test coordinates equals positive") {
    new TestNodesAndRels {
      assert((coordinates1Copy equals coordinates1) === true)
    }
  }

  test("test coordinates comesBefore") {
    new TestNodesAndRels {
      assert((coordinates1 comesBefore coordinates2) === true)
    }
  }

  test("test coordinates toString") {
    new TestNodesAndRels {
      assert(coordinates1.toString === "(150, 301, reverse)")
    }
  }

  test("test boundaries required organism") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        Boundaries(gene1, gene4)
      }
      assert(thrown.getMessage === "requirement failed: Genes in the operon must be located on the same strand!")
    }
  }

  test("test boundaries required ccp") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        Boundaries(gene1, gene5)
      }
      assert(thrown.getMessage === "requirement failed: Genes must be located on the same CCP!")
    }
  }

  test("test boundaries required strand") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        Boundaries(gene1, gene4)
      }
      assert(thrown.getMessage === "requirement failed: Genes in the operon must be located on the same strand!")
    }
  }

  test("test boundaries required comesBefore") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        Boundaries(gene2, gene1)
      }
      assert(thrown.getMessage === "requirement failed: Start gene coordinate cannot have bigger value than end gene coordinate!")
    }
  }

  test("test boundaries getFirstGene") {
    new TestNodesAndRels {
      assert(boundaries1.getFirstGene === gene1)
    }
  }

  test("test boundaries getLastGene") {
    new TestNodesAndRels {
      assert(boundaries1.getLastGene === gene2)
    }
  }

  test("test boundaries getStrand") {
    new TestNodesAndRels {
      assert(boundaries1.getStrand === Strand.reverse)
    }
  }

  test("test boundaries equals negative") {
    new TestNodesAndRels {
      assert((boundaries1 equals boundaries2) === false)
    }
  }

  test("test boundaries equals positive") {
    new TestNodesAndRels {
      assert((boundaries1Copy equals boundaries1) === true)
    }
  }

  test("test sequence getSequence") {
    new TestNodesAndRels {
      assert(sequence3.getSequence === "MPKIVILPHQDLCPDGAVLEANSGETILDAALRNGIEIEHACEKSCACTTCHCIVREGFDSLPESSEQEDDMLDKAWGLEPESRLSCQARVTDEDLVVEIPRYTINHAREH")
    }
  }

  test("test sequence getMD5") {
    new TestNodesAndRels {
      assert(sequence3.getMD5 === "65886732974B5F7A601190682A3B3EF5")
    }
  }

  test("test sequence countMD5") {
    new TestNodesAndRels {
      assert(sequence3.countMD5 === "65886732974B5F7A601190682A3B3EF5")
    }
  }

  test("test sequence getSimilarities") {
    new TestNodesAndRels {
      assert(sequence3.getSimilarities === List(similarity1))
    }
  }

  test("test sequence getSimilarities backward") {
    new TestNodesAndRels {
      assert(sequence2.getSimilarities === List(Similarity(sequence3, 1e-12, 78.5)))
    }
  }

  test("test sequence equals negative") {
    new TestNodesAndRels {
      assert((sequence3 equals sequence1) === false)
    }
  }

  test("test sequence equals positive") {
    new TestNodesAndRels {
      assert((sequence1Copy equals sequence1) === true)
    }
  }

  test("test sequence addSimilarity") {
    new TestNodesAndRels {
      sequence3.addSimilarity(similarity2)
      assert(sequence3.getSimilarities === List(similarity2, similarity1))
    }
  }

  test("test sequence sequenceLength") {
    new TestNodesAndRels {
      assert(sequence3.getLength === 111)
    }
  }

  test("test sequence default properties") {
    new TestNodesAndRels {
      assert(sequence3.getProperties === Map())
    }
  }

  test("test similarity required identity") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        Similarity(sequence1, 1E-10, 101.5)
      }
      assert(thrown.getMessage === "requirement failed: Identity cannot be more than 100%.")
    }
  }

  test("test term getLabels") {
    new TestNodesAndRels {
      assert(geneTerm.getLabels === List("Term"))
    }
  }

  test("test term getText") {
    new TestNodesAndRels {
      assert(geneTerm.getText === "Gene standard name")
    }
  }

  test("test term equals negative") {
    new TestNodesAndRels {
      assert((geneTerm equals geneTerm2) === false)
    }
  }

  test("test term equals positive") {
    new TestNodesAndRels {
      assert((geneTermCopy equals geneTerm) === true)
    }
  }

  test("test organism getLabels") {
    new TestNodesAndRels {
      assert(organism.getLabels === List("Organism"))
    }
  }

  test("test organism getName") {
    new TestNodesAndRels {
      assert(organism.getName === "Eschrichia coli")
    }
  }

  test("test organism getTaxon") {
    new TestNodesAndRels {
      assert(organism.getTaxon === taxon)
    }
  }

  test("test organism setTaxon") {
    new TestNodesAndRels {
      organismCopy.setTaxon(taxon2)
      assert(organismCopy.getTaxon === taxon2)
    }
  }

  test("test organism equals negative") {
    new TestNodesAndRels {
      assert((organism equals organism2) === false)
    }
  }

  test("test organism equals positive") {
    new TestNodesAndRels {
      assert((organismCopy equals organism) === true)
    }
  }

  test("test organism getSource") {
    new TestNodesAndRels {
      assert(organism.getSource === List("GenBank"))
    }
  }

  test("test taxon getLabels") {
    new TestNodesAndRels {
      assert(taxon.getLabels === List("Taxon"))
    }
  }

  test("test taxon getTaxonType") {
    new TestNodesAndRels {
      assert(taxon.getTaxonType === TaxonType.family)
    }
  }

  test("test taxon getTaxID") {
    new TestNodesAndRels {
      assert(taxon.getTaxID === -1)
    }
  }


  test("test taxon equals positive") {
    new TestNodesAndRels {
      assert((taxonCopy equals taxon) === true)
    }
  }

  test("test taxon equals negative") {
    new TestNodesAndRels {
      assert((taxon equals taxon2) === false)
    }
  }

  test("test polypeptide getName") {
    new TestNodesAndRels {
      assert(poly1.getName === "Test poly 1")
    }
  }

  test("test polypeptide getLabels") {
    new TestNodesAndRels {
      assert(poly1.getLabels === List("Polypeptide", "Peptide", "BioEntity"))
    }
  }

  test("test polypeptide getGene") {
    new TestNodesAndRels {
      assert(poly1.getGene === gene1)
    }
  }

  test("test polypeptide getOrganism") {
    new TestNodesAndRels {
      assert(poly1.getOrganism === organism)
    }
  }

  test("test polypeptide getSource") {
    new TestNodesAndRels {
      assert(poly1.getSource === List("GenBank", "MetaCyc"))
    }
  }

  test("test polypeptide getSeq") {
    new TestNodesAndRels {
      assert(poly1.getSeq === sequence1)
    }
  }

  test("test polypeptide getXrefs") {
    new TestNodesAndRels {
      assert(poly1.getXrefs === List(xref1))
    }
  }

  test("test polypeptide getTerms") {
    new TestNodesAndRels {
      assert(poly1.getTerms === List(term1))
    }
  }

  test("test polypeptide equals positive") {
    new TestNodesAndRels {
      assert((poly1Copy equals poly1) === true)
    }
  }

  test("test polypeptide equals negative") {
    new TestNodesAndRels {
      assert((poly1 equals poly2) === false)
    }
  }

  test("test compound getLabels") {
    new TestNodesAndRels {
      assert(compound1.getLabels === List("Compound"))
    }
  }

  test("test compound getName") {
    new TestNodesAndRels {
      assert(compound1.getName === "oxybuprocaine")
    }
  }

  test("test compound getInchi") {
    new TestNodesAndRels {
      assert(compound1.getInchi === "InChI=1S/C17H28N2O3/c1-4-7-11-21-16-13-14(8-9-15(16)18)17(20)22-12-10-19(5-2)6-3/h8-9,13H,4-7,10-12,18H2,1-3H3")
    }
  }

  test("test compound getSmiles") {
    new TestNodesAndRels {
      assert(compound1.getSmiles === "CCCCCCCCCC(CCCCCCCCCCCC([O-])=O)OC1(OC(C(C(C1OC2(OC(C(C(C2O)O)O)COC(C)=O))O)O)COC(=O)C)")
    }
  }

  test("test compound getXrefs") {
    new TestNodesAndRels {
      assert(compound1.getXrefs === List(xref1))
    }
  }

  test("test compound setXrefs") {
    new TestNodesAndRels {
      compound1.setXrefs(xref2)
      assert(compound1.getXrefs === List(xref2, xref1))
    }
  }

  test("test compound equals positive") {
    new TestNodesAndRels {
      assert((compound1Copy equals compound1) === true)
    }
  }

  test("test compound equals negative") {
    new TestNodesAndRels {
      assert((compound1 equals compound2) === false)
    }
  }

  test("test similarity getSequence") {
    new TestNodesAndRels {
      assert(similarity1.getSequence === sequence2)
    }
  }

  test("test similarity getEvalue") {
    new TestNodesAndRels {
      assert(similarity1.getEvalue === 1.0E-12)
    }
  }

  test("test similarity getIdentity") {
    new TestNodesAndRels {
      assert(similarity1.getIdentity === 78.5)
    }
  }

  test("test similarity equals positive") {
    new TestNodesAndRels {
      assert((similarity1Copy equals similarity1) === true)
    }
  }

  test("test similarity equals negative") {
    new TestNodesAndRels {
      assert((similarity1 equals similarity2) === false)
    }
  }

  test("test operon getLabels") {
    new TestNodesAndRels {
      assert(operon.getLabels === List("Operon", "BioEntity", "DNA"))
    }
  }

  test("test operon getName") {
    new TestNodesAndRels {
      assert(operon.getName === "So operonious")
    }
  }

  test("test operon getOrganism") {
    new TestNodesAndRels {
      assert(operon.getOrganism === organism)
    }
  }

  test("test operon getStandardName") {
    new TestNodesAndRels {
      assert(operon.getStandardName === operonTerm)
    }
  }

  test("test operon getTUs") {
    new TestNodesAndRels {
      assert(operon.getTUs === List(tu1))
    }
  }

  test("test operon addTU") {
    new TestNodesAndRels {
      operon.addTU(tu2)
      assert(operon.getTUs === List(tu2, tu1))
    }
  }

  test("test tu getLabels") {
    new TestNodesAndRels {
      assert(tu1.getLabels === List("TU", "BioEntity", "DNA"))
    }
  }

  test("test tu getName") {
    new TestNodesAndRels {
      assert(tu1.getName === "Very transcriptonal")
    }
  }

  test("test tu consistsOf") {
    new TestNodesAndRels {
      assert(tu1.consistsOf === List(promoter, gene1))
    }
  }

  test("test tu getStandardName") {
    new TestNodesAndRels {
      assert(tu1.getStandardName === tuTerm1)
    }
  }

  test("test tu participatesIn") {
    new TestNodesAndRels {
      val thrown = intercept[Exception] {
        tu1.participatesIn
      }
      assert(thrown.getMessage === "Not implemented yet!")
    }
  }

  test("test tu getOperon") {
    new TestNodesAndRels {
      assert(tu1.getOperon === operon)
    }
  }

  test("test tu getOrganism") {
    new TestNodesAndRels {
      assert(tu1.getOrganism === organism)
    }
  }

  test("test rna getOrganism") {
    new TestNodesAndRels {
      assert(rna1.getOrganism === organism)
    }
  }

  test("test rna getName") {
    new TestNodesAndRels {
      assert(rna1.getName === "5S ribosomal RNA")
    }
  }

  test("test rna getLabels") {
    new TestNodesAndRels {
      assert(rna1.getLabels === List("RNA", "BioEntity", "sRNA"))
    }
  }

  test("test rna getSource") {
    new TestNodesAndRels {
      assert(rna1.getSource === List("GenBank"))
    }
  }

  test("test rna getGene") {
    new TestNodesAndRels {
      assert(rna1.getGene === gene4)
    }
  }

  test("test rna equals negative") {
    new TestNodesAndRels {
      assert((rna1 equals rna3) === false)
    }
  }

  test("test rna equals positive") {
    new TestNodesAndRels {
      assert((rna1 equals rna1Copy) === true)
    }
  }

  test("test reactant getLabels 1") {
    new TestNodesAndRels {
      assert(reactant1.getLabels === List("BiochemicalReactant", "To_check"))
    }
  }

  test("test reactant getLabels 2") {
    new TestNodesAndRels {
      assert(reactant2.getLabels === List("BiochemicalReactant"))
    }
  }

  test("test reactant equals positive") {
    new TestNodesAndRels {
      assert((reactant1 equals reactant1Copy) === true)
    }
  }

  test("test reactant equals negative") {
    new TestNodesAndRels {
      assert((reactant1 equals reactant2) === false)
    }
  }

  test("test reaction equals positive") {
    new TestNodesAndRels {
      assert((reaction1 equals reaction1Copy) === true)
    }
  }

  test("test reaction equals negative") {
    new TestNodesAndRels {
      assert((reaction1 equals reaction2) === false)
    }
  }

}
