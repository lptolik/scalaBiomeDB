package BioGraph

import java.io.File

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.{BiomeDBRelations, TransactionSupport, utilFunctionsObject}

import scala.io.Source
import scala.collection.JavaConverters._

object PEGIDsUploadApp extends App with TransactionSupport  {
  val bp = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/" +
    "agora/patric-fams-2016-0904-reduced2/"

//  val localDB = new File("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/1500_organisms/data/graph.db")
//  val fastaPath = bp + "families.nr/nr.0001"
  val localDB = new File(args(0))
  val fastaPaths = args.drop(1)
  val db = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)

  val taxonIds: Set[Int] = transaction(db) {
    val q = "MATCH (n:Taxon)<-[:IS_A]-(:Organism) RETURN n.tax_id as taxonId"
    db.execute(q).columnAs[Int]("taxonId").asScala.toSet
  }

  println("started parsing fasta")

  val allSeqs = readFasta(fastaPaths)
  println(s"all seqs filtered")

  transaction(db) {
    val dbNode = DBNode("SEED")
    dbNode.upload(db)

    val addedCount = allSeqs
      .grouped(10000)
      .map { gr =>
        gr.groupBy(_.taxonId)
          .filter(_._1 >= 0)
          .map { case (taxonId, taxonSeqs) =>
            val q = s"MATCH (n:Taxon {tax_id: $taxonId})<-[:IS_A]-(o:Organism)<-[:PART_OF]-" +
              s"(p:Polypeptide)-[:IS_A]-(s:AA_Sequence) RETURN ID(p) as ppNodeId, s.md5 as md5"

            val map = db.execute(q).asScala.map { jMap =>
              val sMap = jMap.asScala
              (sMap("md5").asInstanceOf[String], sMap("ppNodeId").asInstanceOf[Long])
            }.toMap

            if (map.nonEmpty) {
              val res = taxonSeqs
                .filter(seq => map.contains(seq.md5))
                .map { seq =>
                  val ppNodeId = map(seq.md5)

                  val xRefNode = XRef(seq.rawId.replace(">", ""), dbNode).upload(db)
                  db.getNodeById(ppNodeId).createRelationshipTo(xRefNode, BiomeDBRelations.evidence)
                }

              println(s"taxon: $taxonId, ${res.size} PEG links added")

              res.size
            } else {
              println(s"taxon: $taxonId, map is empty, 0 PEG links added")
              0
            }
          }.sum
        }.sum

    println(s"Added to all organisms: $addedCount PEG links added")
  }

  private def readFasta(paths: Seq[String]): Iterator[FastaSequence] = {
    val linesIter = fastaPaths.iterator.flatMap(path => Source.fromFile(path).getLines().filter(_.nonEmpty))

    var rawId: String = ""
    var taxonId: Int = -1
    var seqAcc: String = ""
    val initSeq = FastaSequence("", -1, "")

    def getNextSeq() = {
      var seq: FastaSequence = initSeq

      while (linesIter.nonEmpty && seq == initSeq) {
        val line = linesIter.next().trim
        if (!line.startsWith(">")) { //accumulate seq
          seqAcc += line
        }
        else if (!taxonIds.contains(taxonId)) { //seq accumulated, taxonId is not in list, go further
          rawId = line
          taxonId = FastaSequence.extractTaxonId(line)
          seqAcc = ""
        }
        else { //seq accumulated, taxonId _is_ in list, add it to res and go further
          seq = FastaSequence(rawId, taxonId, utilFunctionsObject.md5ToString(seqAcc))
          rawId = line
          taxonId = FastaSequence.extractTaxonId(line)
          seqAcc = ""
        }
      }

      seq
    }

    new Iterator[FastaSequence] {
      override def hasNext = linesIter.hasNext

      override def next() = getNextSeq()
    }
  }
}

case class FastaSequence(rawId: String, taxonId: Int, md5: String)

object FastaSequence {
  def takeFuckWhile(s: String): Int = {
    var res = s(0) - 48
    var i = 1
    var next: Char = '0'

    while(i < s.length) {
      next = s(i)
      if (next != '.')
        res = res * 10 + next - 48
      else
        return res

      i = i + 1
    }

    res
  }

  def extractTaxonId(rawId: String): Int = {
    try {
      takeFuckWhile(rawId.drop(5))
    } catch {
      case _ => -1
    }
  }
}
