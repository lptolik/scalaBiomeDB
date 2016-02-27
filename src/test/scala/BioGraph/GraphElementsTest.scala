package BioGraph

import org.scalatest.FunSuite

/**
  * Created by artem on 11.02.16.
  */
class GraphElementsTest extends FunSuite {

  trait TestNodesAndRels {
    val geneTerm = new Term("Gene standard name")
    val promoterTerm = new Term("Gene standard name")
    val organism = new Organism("Eschrichia coli")
    val uniprot = new DBNode("UniProt", Map("some property" -> "U123"))
    val xref = new XRef("NZ_ACKO02000005", uniprot, Map("db_id" -> "NZ_ACKO02000005_GenBank"))
    val link = new LinkTo(xref, uniprot)
    val chromosome = new Chromosome("Corynebacterium glutamicum ATCC 13032, complete genome.", SourceType.MetaCyc, DNAType.circular, organism, 3282708, Map("some property" -> "works fine"))
    val plasmid = new Plasmid("Listeria grayi DSM 20601", organism = organism)
    val contig = new Contig("Blautia hansenii DSM 20583 genomic scaffold Scfld1, whole genome shotgun sequence.", organism = organism)
//    val gene = new Gene("gene name", Map())
    val coordinates = new Coordinates(150, 301, Strand.reverse)
    val gene = new Gene("Super name", coordinates, plasmid, geneTerm, organism, Map("source" -> "GenBank"))
    val evidence = new Evidence(gene, xref)
//    val chrom = new Chromosome("E. coli chromosome", SourceType.genBank, ChromType.circular, , Map("length" -> 120500))
    val promoter = new Promoter("ydjFp1", new Coordinates(1854924, 1854924, Strand.unknown), plasmid, organism, 1852948, promoterTerm)
    val terminator = new Terminator(new Coordinates(4633111, 4633144, Strand.forward), plasmid)
    val miscFeature = new MiscFeature(new Coordinates(1560, 6301, Strand.reverse), plasmid)
    val miscStructure = new MiscStructure(new Coordinates(3020, 3101, Strand.forward), plasmid)
    val mobileElement = new MobileElement("So mobile", new Coordinates(3020, 3101, Strand.forward), plasmid)
//    val m = Map("name" -> "my name")
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
      assert(List(evidence.startNode, evidence.endNode) === List(gene, xref))
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
      assert(chromosome.getSource === SourceType.MetaCyc)
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
      assert(gene.getLabels === List("BioEntity", "Feature", "Gene"))
    }
  }

  test("name gene test") {
    new TestNodesAndRels {
      assert(gene.getName === "Super name")
    }
  }

  test("coordinates gene test") {
    new TestNodesAndRels {
      assert(gene.getCoordinates === Coordinates(150, 301, Strand.reverse))
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

}
