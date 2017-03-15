package BioGraph

import utilFunctions.BlastUtil

/**
  * Created by artem on 28.12.16.
  */
object BlastTaskMaker extends App{
  def main(configurationFilename: String): Unit = {
//    val dbPathLocal = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
//    val dbPathLocal = "/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/"
    val dbPathRemote = "/var/lib/neo4j_2.3.1_240_bacs/neo4j-community-2.3.1/data/graph.db"

    val conf = utilFunctions.utilFunctionsObject.readConfigurationFile(configurationFilename)
    val dbPathLocal = conf(0)
    val outputPolySeqFile = conf(1)
    val outputGeneSeqFile = conf(2)
    val blastTool = new BlastUtil(dbPathLocal)
    val polySequences = blastTool.getAllAASequencesNodes
    val genesSequences = blastTool.getAllDNASequencesNodes

//    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/ecoli_w_and_k12.fasta")

    blastTool.makeSequencesFastaFile(genesSequences, outputGeneSeqFile, true)
    blastTool.makeSequencesFastaFile(polySequences, outputPolySeqFile, true)

//    blastTool.makeSequencesFastaFile(genesSequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_gene_byMD5.fasta", true)
//    blastTool.makeSequencesFastaFile(genesSequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_gene_byID.fasta", false)

//    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_poly_byMD5.fasta", true)
//    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/2017/staphylococcus/staphylococcusDB_poly_byID.fasta", false)

  }
//  main()
}
