package utilFunctions


import utilFunctions.utilFunctionsObject

/**
  * Created by artem on 16.03.17.
  */
object UtilObjectFunctionsApp extends App{
  def main(): Unit = {
    utilFunctionsObject.addMD5ToFastaHeaders(
      "/home/artem/work/DataBasesDumps/RefSeq/ref_prok_rep_genomes.05/output.fasta",
      "/home/artem/work/DataBasesDumps/RefSeq/ref_prok_rep_genomes.05/count.fasta")
  }
  main()
}
