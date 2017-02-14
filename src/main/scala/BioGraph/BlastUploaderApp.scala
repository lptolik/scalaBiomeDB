package BioGraph

import utilFunctions.BlastUtil
/**
  * Created by artem on 01.05.16.
  */
object BlastUploaderApp extends App{
  def main(): Unit = {
//    val dbPathLocal = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
    val dbPathLocal = "/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/"
    val dbPathRemote = "/var/lib/neo4j_2.3.1_240_bacs/neo4j-community-2.3.1/data/graph.db"
    val blastTool = new BlastUtil(dbPathLocal)
//    blastTool.makeOuterBlast("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/outer_blast_result_ecoli_w_and_k12.txt", 0)
//    blastTool.makeInnerBlast("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/inner_blast_result_ecoli_w_and_k12.txt", 0)
    blastTool.makePolyOuterBlast("/home/artem/work/2017/staphylococcus/staphylococcusDB_byMD5_outer_blast_result.txt", 0, true)
    blastTool.makePolyInnerBlast("/home/artem/work/2017/staphylococcus/staphylococcusDB_byMD5_inner_blast_result.txt", 0, true)
  }
  main()
}
