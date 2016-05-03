package BioGraph

import utilFunctions.BlastUtil
/**
  * Created by artem on 01.05.16.
  */
object BlastUploaderApp {
  def main(): Unit = {
    val dbPathRemote = "/var/lib/neo4j_2.3.1_240_bacs/neo4j-community-2.3.1/data/graph.db"
    val blastTool = new BlastUtil(dbPathRemote)
    blastTool.makeInnerBlast("/home/jane/BLAST_240_bacs/bacs_240_inner_blast_output.txt", 0)
  }
}
