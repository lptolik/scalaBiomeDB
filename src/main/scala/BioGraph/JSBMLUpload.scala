package BioGraph

import java.io.File

/**
  * Created by artem on 14.07.16.
  */
object JSBMLUpload {
  def main(args: Array[String]): Unit = {
    val dataBaseFile = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val jsbml = new JSBMLUtil(dataBaseFile)
//    BMID000000065401.xml BIOMD0000000051.xml
    val model = jsbml.processSBMLFile("/home/artem/work/2016/JSBML/iWFL_1372.xml/iWFL_1372.xml")
    jsbml.uploadModels(model)
  }
}
