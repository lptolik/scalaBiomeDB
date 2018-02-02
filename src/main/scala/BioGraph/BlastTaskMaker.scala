package BioGraph

import org.neo4j.cypher.internal.frontend.v2_3.SemanticDirection.INCOMING
import utilFunctions.{BiomeDBRelations, BlastUtil}
import org.neo4j.graphdb.Direction.{INCOMING, OUTGOING}
import scala.collection.JavaConverters._

/**
  * Created by artem on 28.12.16.
  */
object BlastTaskMaker extends App{
  def main(configurationFilename: String = "/home/jane/graph_new_release/microbiomeBlastTaskMakerConf.txt"): Unit = {
//    val dbPathLocal = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
//    val dbPathLocal = "/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/"
//    val dbPathRemote = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"

    val conf = utilFunctions.utilFunctionsObject.readConfigurationFile(configurationFilename)
    val dbPathLocal = conf(0)
    val outputPolySeqFile = conf(1)
    val outputGeneSeqFile = conf(2)
    val byMD5 = conf(3).toBoolean
    val blastTool = new BlastUtil(dbPathLocal)
//    val blastTool = new BlastUtil(dbPathRemote)
    val polySequences = blastTool.getAllAASequencesNodes
    val genesSequences = blastTool.getAllDNASequencesNodes

//    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/ecoli_w_and_k12.fasta")

//    val outputPolySeqFile = "/home/artem/work/2017/Timofei/k12_poly_byMD5.fasta"
//    val outputPolySeqFile = "/home/jane/graph_new_release/UBLAST/k12_poly_byMD5.fasta"
//    val outputGeneSeqFile = "/home/artem/work/2017/Timofei/k12_gene_byMD5.fasta"

    //blastTool.makeSequencesFastaFile(polySequences, outputPolySeqFile, byMD5=true)
    //blastTool.makeSequencesFastaFile(genesSequences, outputGeneSeqFile, byMD5=true)

//    val ecoliPolys = blastTool.makeOneOrganismBlastTask("Escherichia coli str. K-12 substr. MG1655", "Polypeptide")
    blastTool.makeSequencesFastaFile(polySequences, outputPolySeqFile, byMD5=byMD5)
    blastTool.makeSequencesFastaFile(genesSequences, outputGeneSeqFile, byMD5=byMD5)

//    blastTool.makeSequencesFastaFile(genesSequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_gene_byMD5.fasta", true)
//    blastTool.makeSequencesFastaFile(genesSequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_gene_byID.fasta", false)

//    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_poly_byMD5.fasta", true)
//    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_poly_byID.fasta", false)

  }
//  main("/home/artem/work/2017/Timofei/blast_task_maker_config.txt")

}
