package BioGraph

import java.io.File

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.{BiomeDBRelations, TransactionSupport, utilFunctionsObject}

import scala.io.Source
import scala.collection.JavaConverters._

object PEGIDsUploadApp extends App with TransactionSupport  {

  def main(configurationFilename: String = "/home/artem/work/2017/Timofei/pegID_upload_config.txt"): Unit = {

    //  val bp = "/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/" +
    //    "agora/patric-fams-2016-0904-reduced2/"
    val conf = utilFunctionsObject.readConfigurationFile(configurationFilename)
//    val bp = "/home/artem/work/2017/Timofei/patric-fams-2016-0904-reduced2/families.nr/"

    //  val localDB = new File("/Users/ramso/Yandex.Disk.localized/Studying/PhD/thesis/pushchino_phd/1500_organisms/data/graph.db")
    val localDB = new File(conf(0))
    //  val fastaPath = bp + "families.nr/nr.0001"
    val fastaPath = conf(1)
    //  val localDB = new File(args(0))
    //  val fastaPath = args(1)
    val dataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(localDB)

    val taxonIds: Set[Int] = transaction(dataBaseConnection) {
      val TaxonToOrganismQuery = "MATCH (n:Taxon)<-[:IS_A]-(:Organism) RETURN n.tax_id as taxonId"
      dataBaseConnection.execute(TaxonToOrganismQuery).columnAs[Int]("taxonId").asScala.toSet
    }

    println("Started parsing fasta")

    val allSeqs = readFasta(fastaPath, taxonIds)
    println(s"All sequences were filtered")

    transaction(dataBaseConnection) {
      val dbNode = DBNode("SEED")
      dbNode.upload(dataBaseConnection)

      val addedPEGLinksCounter = allSeqs
        .grouped(10000)
        .map {
          gr =>
            gr.groupBy(_.taxonId)
              .filter(_._1 >= 0)
              .map {
                case (taxonId: Int, taxonSeqs: Seq[FastaSequence]) => {
                  val matchingTaxonToPolypeptideQuery =
                    s"MATCH (n:Taxon {tax_id: $taxonId})<-[:IS_A]-(o:Organism)<-[:PART_OF]-" +
                      s"(p:Polypeptide)-[:IS_A]-(s:AA_Sequence) RETURN ID(p) as ppNodeId, s.md5 as md5"

                  val md5ToPolyIDDict = dataBaseConnection
                    .execute(matchingTaxonToPolypeptideQuery)
                    .asScala
                    .map { javaPolyIDmd5Pair =>
                      val scalaPolyIDmd5Pair = javaPolyIDmd5Pair.asScala
                      (scalaPolyIDmd5Pair("md5").asInstanceOf[String], scalaPolyIDmd5Pair("ppNodeId").asInstanceOf[Long])
                    }
                    .toMap

                  if (md5ToPolyIDDict.nonEmpty) {
                    val res = taxonSeqs
                      .filter(seq => md5ToPolyIDDict.contains(seq.md5))
                      .map { seq =>
                        val ppNodeId = md5ToPolyIDDict(seq.md5)
                        val xRefNode = XRef(seq.rawId.replace(">", ""), dbNode).upload(dataBaseConnection)
                        dataBaseConnection.getNodeById(ppNodeId).createRelationshipTo(xRefNode, BiomeDBRelations.evidence)
                      }

                    println(s"For taxon: $taxonId, ${res.size} PEG links added")

                    res.size
                  }
                  else {
                    println(s"Taxon: $taxonId, map is empty, 0 PEG links added")
                    0
                  }
                }
              }.sum
        }.sum

      println(s"Added to all organisms: $addedPEGLinksCounter PEG links added")
    }
  }

  private def readFasta(path: String, taxonIds: Set[Int]): Iterator[FastaSequence] = {
    val linesIter = Source
      .fromFile(path)
      .getLines()
      .filter(_.nonEmpty)

    var rawId: String = ""
    var taxonId: Int = -1
    var seqAcc: String = ""
    val initSeq = FastaSequence("", -1, "")

    def getNextSeq = {
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
//          not clear when this condition is working
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

      override def next() = getNextSeq
    }
  }

  case class FastaSequence(rawId: String, taxonId: Int, md5: String)

  object FastaSequence {
    def readLineByLetter(s: String): Int = {
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
        readLineByLetter(rawId.drop(5))
      } catch {
        case _ => -1
      }
    }
  }
//  main("/home/artem/work/2017/Timofei/pegID_upload_config.txt")
}

