package BioGraph

import utilFunctions.BlastUtil

/**
  * Created by artem on 28.12.16.
  */
object BlastTaskMaker extends App{
  def main(): Unit = {
    val dbPathLocal = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
    val dbPathRemote = "/var/lib/neo4j_2.3.1_240_bacs/neo4j-community-2.3.1/data/graph.db"
    val blastTool = new BlastUtil(dbPathLocal)
    val polySequences = blastTool.getAllSequencesNodes
    blastTool.makeSequencesFastaFile(polySequences, "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/ecoli_w_and_k12.fasta")

  }
  main()
}
