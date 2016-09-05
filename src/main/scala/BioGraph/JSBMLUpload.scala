package BioGraph

import java.io.File

/**
  * Created by artem on 14.07.16.
  */
object JSBMLUpload {
  def main(args: Array[String]): Unit = {
//    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val dataBaseFile = new File("/var/lib/neo4j_2.3.1_240_bacs_scala/neo4j-community-2.3.1/data/graph.db")
    val jsbml = new JSBMLUtil(dataBaseFile)
//    BMID000000065401.xml BIOMD0000000051.xml
//    val model = jsbml.processSBMLFile("/home/artem/work/2016/JSBML/BMID000000065401.xml")
    val model = jsbml.processSBMLFile("/home/jane/iWFL_1372.xml")
    jsbml.uploadModels(model)
  }
}
