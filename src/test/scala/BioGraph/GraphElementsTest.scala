package BioGraph

import org.scalatest.FunSuite
import utilFunctions.utilFunctionsObject

/**
  * Created by artem on 11.02.16.
  */
class GraphElementsTest extends FunSuite {

  trait TestNodesAndRels {
    val geneTerm = new Term("Gene standard name")
    val geneTerm2 = new Term("Such standard")
    val geneTerm3 = new Term("The most common gene name")
    val geneTerm4 = new Term("The most common gene name on forward strand")
    val geneTerm5 = new Term("The most common gene name in another organism")

    val promoterTerm = new Term("Gene standard name")

    val organism = new Organism("Eschrichia coli")

    val uniprot = new DBNode("UniProt", Map("some property" -> "U123"))
    val xref = new XRef("NZ_ACKO02000005", uniprot, Map("db_id" -> "NZ_ACKO02000005_GenBank"))
    val link = new LinkTo(xref, uniprot)

    val chromosome = new Chromosome("Corynebacterium glutamicum ATCC 13032, complete genome.", ReferenceSource.MetaCyc, DNAType.circular, organism, 3282708, Map("some property" -> "works fine"))
    val plasmid = new Plasmid("Listeria grayi DSM 20601", organism = organism)
    val contig = new Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld1, whole genome shotgun sequence.", organism = organism)

    val coordinates1 = new Coordinates(150, 301, Strand.reverse)
    val coordinates2 = new Coordinates(2460, 2874, Strand.reverse)
    val coordinates3 = new Coordinates(750, 1013, Strand.reverse)
    val coordinates4 = new Coordinates(1753, 1917, Strand.forward)
    val coordinates5 = new Coordinates(524, 729, Strand.reverse)

    val gene1 = new Gene("Super name", coordinates1, plasmid, geneTerm, organism, Map("source" -> "GenBank"))
    val gene2 = new Gene("Such name", coordinates2, plasmid, geneTerm2, organism)
    val gene3 = new Gene("Super long gene name", coordinates3, plasmid, geneTerm3, organism, Map("source" -> "GenBank"))
    val gene4 = new Gene("Super long gene name on forward strand", coordinates4, plasmid, geneTerm4, organism, Map("source" -> "GenBank"))
    val gene5 = new Gene("Super long gene name in another organism", coordinates4, contig, geneTerm4, organism, Map("source" -> "GenBank"))

    val evidence = new Evidence(gene1, xref)

    val promoter = new Promoter("ydjFp1", new Coordinates(1854924, 1854924, Strand.unknown), plasmid, organism, 1852948, promoterTerm)
    val terminator = new Terminator(new Coordinates(4633111, 4633144, Strand.forward), plasmid)
    val miscFeature = new MiscFeature(new Coordinates(1560, 6301, Strand.reverse), plasmid)
    val miscStructure = new MiscStructure(new Coordinates(3020, 3101, Strand.forward), plasmid)
    val mobileElement = new MobileElement("So mobile", new Coordinates(3020, 3101, Strand.forward), plasmid)

    val boundaries1 = new Boundaries(gene1, gene2)
    val boundaries2 = new Boundaries(gene3, gene2)

    val sequence1 = new Sequence("MSIQLNGINCFYGAHQALFDITLDCPQGETLVLLGPSGAGKSSLLRVLNLLEMPRSGTLNIAGNHFDFTKTPSDKAIRDLRRNVGMVFQQYNLWPHLTVQQNLIEAPCRVLGLSKDQALARAEKLLERLRLKPYSDRYPLHLSGGQQQRVAIARALMMEPQVLLFDEPTAALDPEITAQIVSIIRELAETNITQVIVTHEVEVARKTASRVVYMENGHIVEQGDASCFTEPQTEAFKNYLSH")
    val sequence2 = new Sequence("MNITATVLLAFGMSMDAFAASIGKGATLHKPKFSEALRTGLIFGAVETLTPLIGWGMGMLASRFVLEWNHWIAFVLLIFLGGRMIIEGFRGADDEDEEPRRRHGFWLLVTTAIATSLDAMAVGVGLAFLQVNIIATALAIGCATLIMSTLGMMVGRFIGSIIGKKAEILGGLVLIGIGVQILWTHFHG")
    val sequence3 = new Sequence("MPKIVILPHQDLCPDGAVLEANSGETILDAALRNGIEIEHACEKSCACTTCHCIVREGFDSLPESSEQEDDMLDKAWGLEPESRLSCQARVTDEDLVVEIPRYTINHAREH")

    val similarity1 = new Similarity(sequence2, 1e-12, 78.5)
    val similarity2 = new Similarity(sequence1, 1e-15, 87.4)
    sequence3.addSimilarity(similarity1)

  }

  test("labels DBNode test") {
    new TestNodesAndRels {
      assert(uniprot.getLabels === List("DB"))
    }
  }

  test("get properties DBNode test") {
    new TestNodesAndRels {
      assert(uniprot.getProperties === Map("some property" -> "U123"))
    }
  }

  test("get name DBNode test") {
    new TestNodesAndRels {
      assert(uniprot.getName === "UniProt")
    }
  }

  test("labels XRef test") {
    new TestNodesAndRels {
      assert(xref.getLabels === List("XRef"))
    }
  }

  test("get properties XRef test") {
    new TestNodesAndRels {
      assert(xref.getProperties === Map("db_id" -> "NZ_ACKO02000005_GenBank"))
    }
  }

  test("get XRef XRef test") {
    new TestNodesAndRels {
      assert(xref.getXRef === "NZ_ACKO02000005")
    }
  }

  test("get DB XRef test") {
    new TestNodesAndRels {
      assert(xref.getDB === uniprot)
    }
  }

  test("get properties link test") {
    new TestNodesAndRels {
      assert(link.getProperties === Map())
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

  test("get labels evidence test") {
    new TestNodesAndRels {
      assert(evidence.getLabel === "EVIDENCE")
    }
  }

  test("get properties evidence test") {
    new TestNodesAndRels {
      assert(evidence.getProperties === Map())
    }
  }

  test("start end nodes evidence test") {
    new TestNodesAndRels {
      assert(List(evidence.startNode, evidence.endNode) === List(gene1, xref))
    }
  }

  test("get properties chromosome test") {
    new TestNodesAndRels {
      assert(chromosome.getProperties === Map("some property" -> "works fine"))
    }
  }

  test("get labels chromosome test") {
    new TestNodesAndRels {
      assert(chromosome.getLabels === List("Chromosome", "BioEntity"))
    }
  }

  test("length chromosome test") {
    new TestNodesAndRels {
      assert(chromosome.getLength === 3282708)
    }
  }

  test("type chromosome test") {
    new TestNodesAndRels {
      assert(chromosome.getType === CCPType.Chromosome)
    }
  }

  test("source chromosome test") {
    new TestNodesAndRels {
//      assert(chromosome.getSource === "MetaCyc")
      assert(chromosome.getSource === ReferenceSource.MetaCyc)
    }
  }

  test("name chromosome test") {
    new TestNodesAndRels {
      assert(chromosome.getName === "Corynebacterium glutamicum ATCC 13032, complete genome.")
    }
  }

  test("get labels plasmid test") {
    new TestNodesAndRels {
      assert(plasmid.getLabels === List("Plasmid", "BioEntity"))
    }
  }

  test("get labels contig test") {
    new TestNodesAndRels {
      assert(contig.getLabels === List("Contig", "BioEntity"))
    }
  }

  test("get labels gene test") {
    new TestNodesAndRels {
      assert(gene1.getLabels === List("BioEntity", "Feature", "Gene"))
    }
  }

  test("name gene test") {
    new TestNodesAndRels {
      assert(gene1.getName === "Super name")
    }
  }

  test("coordinates gene test") {
    new TestNodesAndRels {
      assert(gene1.getCoordinates === Coordinates(150, 301, Strand.reverse))
    }
  }

  test("get labels promoter test") {
    new TestNodesAndRels {
      assert(promoter.getLabels === List("Promoter", "BioEntity", "Feature", "DNA"))
    }
  }

  test("name promoter test") {
    new TestNodesAndRels {
      assert(promoter.getName === "ydjFp1")
    }
  }

  test("get labels terminator test") {
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

  test("name mobileElement test") {
    new TestNodesAndRels {
      assert(mobileElement.getName === "So mobile")
    }
  }

  test("test read blast result") {
    new TestNodesAndRels {
      val blastedSequences: Map[Int, Sequence] = utilFunctionsObject.readInsideBlastResultFile("/home/artem/work/reps/GenBank/biome_api/biome/load/genbank/cross_blast_scala_text.txt")
      assert(blastedSequences.size === 242)
    }
  }

  test("test coordinates required start and end") {
    new TestNodesAndRels {
      val thrown = intercept[Exception]{
        new Coordinates(101, 100, Strand.forward)
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

  test("test coordinates equals") {
    new TestNodesAndRels {
      assert((coordinates1 equals coordinates2) === false)
    }
  }

  test("test coordinates comesBefore") {
    new TestNodesAndRels {
      assert((coordinates1 comesBefore coordinates2) === true)
    }
  }

  test("test boundaries required organism") {
    new TestNodesAndRels {
      val thrown = intercept[Exception]{
        new Boundaries(gene1, gene4)
      }
      assert(thrown.getMessage === "requirement failed: Genes in the operon must be located on the same strand!")
    }
  }

  test("test boundaries required ccp") {
    new TestNodesAndRels {
      val thrown = intercept[Exception]{
        new Boundaries(gene1, gene5)
      }
      assert(thrown.getMessage === "requirement failed: Genes must be located on the same CCP!")
    }
  }

  test("test boundaries required strand") {
    new TestNodesAndRels {
      val thrown = intercept[Exception]{
        new Boundaries(gene1, gene4)
      }
      assert(thrown.getMessage === "requirement failed: Genes in the operon must be located on the same strand!")
    }
  }

  test("test boundaries required comesBefore") {
    new TestNodesAndRels {
      val thrown = intercept[Exception]{
        new Boundaries(gene2, gene1)
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

  test("test boundaries equals") {
    new TestNodesAndRels {
      assert((boundaries1 equals boundaries2) === false)
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

  test("test sequence equals") {
    new TestNodesAndRels {
      assert((sequence3 equals sequence1) === false)
    }
  }

  test("test sequence addSimilarity") {
    new TestNodesAndRels {
      sequence3.addSimilarity(similarity2)
      assert(sequence3.getSimilarities === List(similarity2, similarity1))
    }
  }

  test("test similarity required identity") {
    new TestNodesAndRels {
      val thrown = intercept[Exception]{
        new Similarity(sequence1, 1E-10, 101.5)
      }
      assert(thrown.getMessage === "requirement failed: Identity cannot be more than 100%.")
    }
  }

}
