package BioGraph

import java.io.File

import org.biojava.nbio.core.sequence.DNASequence
import org.neo4j.graphdb
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import utilFunctions.BiomeDBRelations
import utilFunctions.utilFunctionsObject._

/**
  * Created by artem on 15.05.16.
  */
object GenBankUploader extends App{
  def main() {
    println("Upload started")
    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/Chlamydia_trachomatis_A2497_complete_genome_ver1.gb")
//    val gbReader = new GenBankUtil("/home/artem/work/reps/GenBank/e_coli_k_12.gb")
    val accessions = gbReader.getAccessionsFromGenBankFile
    val inits = accessions.values.map(gbReader.getInitialData)
    val setOfFeatures = accessions.values.map(gbReader.getFeatures).iterator
    val setOfOrganismsNodes = accessions.values.map(gbReader.getInitialData)
    val setOfInitialOrganismNodes = setOfOrganismsNodes.iterator
    val zipFeaturesAndInitialOrganismData = setOfInitialOrganismNodes zip setOfFeatures

    val readGenBankObjects = zipFeaturesAndInitialOrganismData.map(pair => gbReader.processFeatures(pair._1)(pair._2))

    val dbPathRemote = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
    val dataBaseFile = new File(dbPathRemote)
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
    val processReadGenBankObjects = readGenBankObjects.map(_.filter((el) => el.toString != "()").flatMap{
      case (a, b, c) => List(a, b, c)
      case (a, b) => List (a, b)
      case (a) => List(a)
    })


    def uploader(inits: Iterable[(Organism, Node with CCP, DNASequence)], features: Iterator[List[Any]]): Unit = {
      if (inits.nonEmpty) {
        val nextOrgAndCCP = inits.head
        val organismNode = nextOrgAndCCP._1.upload(graphDataBaseConnection)
        val ccpNode = nextOrgAndCCP._2.upload(graphDataBaseConnection)

        features.next().foreach({case elem: Node => elem.upload(graphDataBaseConnection)})
        uploader(inits.tail, features)
      }
    }
    uploader(setOfOrganismsNodes, processReadGenBankObjects)
    println("Upload finished")

  }
  main()
}
