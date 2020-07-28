package BioGraph

import utilFunctions.BlastUtil
/**
  * Created by artem on 01.05.16.
  */
object BlastUploaderApp extends App{
  def main(configurationFilename: String = "/home/artem/work/2017/Timofei/blast_upload_config.txt"): Unit = {
    val dbPathLocal = "/Users/lptolik/Documents/Projects/Liverpool/Penicillium/neo4j/penicillium/data/graph.db"
//    val dbPathLocal = "/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db"
//    val dbPathLocal = "/home/artem/work/2017/staphylococcus/neo4j-community-2.3.1/data/graph.db/"
    val dbPathRemote = "/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db"
    val staphylococcusDB = "/var/lib/neo4j_staphylococcus/neo4j-community-2.3.1/data/graph.db"

    val conf = utilFunctions.utilFunctionsObject.readConfigurationFile(configurationFilename)
    val dbPath = conf(0)

    val blastTool = new BlastUtil(dbPath)

    blastTool.makePolyOuterBlastByMD5(conf(1), 0)
    blastTool.makePolyInnerBlastByMD5(conf(2), 0)
    blastTool.makeGeneOuterBlastByMD5(conf(3), 0)
    blastTool.makeGeneInnerBlastByMD5(conf(4), 0)

    blastTool.makePolyOuterBlastByID(conf(5), 0)
    blastTool.makePolyInnerBlastByID(conf(6), 0)
    blastTool.makeGeneOuterBlastByID(conf(7), 0)
    blastTool.makePolyInnerBlastByID(conf(8), 0)
//    blastTool.makeOuterBlast("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/outer_blast_result_ecoli_w_and_k12.txt", 0)
//    blastTool.makeInnerBlast("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/inner_blast_result_ecoli_w_and_k12.txt", 0)
//    blastTool.makePolyOuterBlastByMD5("/home/artem/work/2017/staphylococcus/staphylococcusDB_byMD5_outer_blast_result.txt", 0)
//    blastTool.makePolyInnerBlastByMD5("/home/artem/work/2017/staphylococcus/staphylococcusDB_byMD5_inner_blast_result.txt", 0)

//    blastTool.makePolyInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/inner_poly_blast_microbiome_1_1.txt", 0)
//    blastTool.makePolyInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/inner_poly_blast_microbiome_1_2.txt", 0)
//    blastTool.makePolyInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/inner_poly_blast_microbiome_2_1.txt", 0)
//    blastTool.makePolyInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/inner_poly_blast_microbiome_2_2.txt", 0)
//    blastTool.makePolyOuterBlastByMD5("/home/jane/graph_new_release/UBLAST/outer_poly_blast_microbiome_1.txt", 0)
//    blastTool.makePolyOuterBlastByMD5("/home/jane/graph_new_release/UBLAST/outer_poly_blast_microbiome_2.txt", 0)

//    blastTool.makePolyOuterBlastByMD5("/home/artem/work/2017/Timofei/k12_poly.txt", 0)
//    blastTool.makePolyOuterBlastByMD5("/home/jane/graph_new_release/UBLAST/outer_poly_blast_k12.txt", 0)
//    blastTool.makePolyInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/inner_poly_blast_k12_1.txt", 0)
//    blastTool.makePolyInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/inner_poly_blast_k12_2.txt", 0)



    //blastTool.makePolyOuterBlastByMD5("/home/artem/work/2017/Timofei/poly_blast_result_outer.txt", 0)
    //blastTool.makePolyInnerBlastByMD5("/home/artem/work/2017/Timofei/poly_blast_result_inner.txt", 0)
    //blastTool.makeGeneInnerBlastByMD5("/home/artem/work/2017/Timofei/gene_blast_result.txt", 0)

//    blastTool.makeGeneInnerBlastByMD5("/home/jane/graph_new_release/UBLAST/staphylococcusDB_gene_byMD5_inner_blast_result.txt", 0)

//    blastTool.makeGeneInnerBlastByMD5("/home/artem/work/2017/staphylococcus/staphylococcusDB_gene_byMD5_inner_blast_result.txt", 0)
  }
  main("/Users/lptolik/Dropbox/Projects/Liverpool/Penicillium/Penicillium/config/penicillium_gems/usearch_tcdb_upload_config.txt")
  //main("/Users/lptolik/Documents/Projects/Liverpool/Penicillium/config/penicillium_gems/usearch_upload_config.txt")
//  main("/Users/lptolik/Documents/Projects/Liverpool/Penicillium/config/penicillium_gems/blast_upload_config.txt")
}
