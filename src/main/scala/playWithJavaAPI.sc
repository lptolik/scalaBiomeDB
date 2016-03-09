import java.util

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.kernel.EmbeddedGraphDatabase
import org.neo4j.graphdb._
import collection.JavaConversions._
import java.io.File

//class StartNeo4jTest {
//  def getNeo4j(): Unit = {
//    val gdb = new GraphDatabaseFactory().newEmbeddedDatabaseBuilder("/home/artem/work/reps/neo4j-community-2.3.1/bin/graph.db")
//  }
//}

//val snt = new StartNeo4jTest


//snt.getNeo4j()
//val db = gdb.newEmbeddedDatabase("/home/artem/work/reps/neo4j-community-2.3.1/")
//val gdb = new GraphDatabaseFactory().newEmbeddedDatabase("/home/artem/work/reps/neo4j-community-2.3.1/")
//val neo: GraphDatabaseService = new EmbeddedGraphDatabase("/home/artem/work/reps/neo4j-community-2.3.1/")
//val source = Source.fromFile("/home/artem/work/reps/neo4j-community-2.3.1/data/graph.db")


val l = DynamicLabel.label("Organism")

class myNeo4j {
  def main() {
    val f = new File("/home/artem/work/reps/neo4j-2.3.1/neo4j-community-2.3.1/data/graph.db")
    val gdb = new GraphDatabaseFactory().newEmbeddedDatabase(f)
    val tx: Transaction = gdb.beginTx()
    try {

      val organismNode = gdb.createNode(DynamicLabel.label("Organism"))
      organismNode.setProperty("name", "E. coli")

      val geneNode = gdb.createNode(DynamicLabel.label("Gene"))
      geneNode.setProperty("name", "qyur")

      val polyNode = gdb.createNode(DynamicLabel.label("Polypeptide"))
      polyNode.setProperty("name", "Qyur")

      val dbNode = gdb.createNode(DynamicLabel.label("DB"))
      dbNode.setProperty("name", "UniProt")

      val termNode = gdb.createNode(DynamicLabel.label("XRef"))
      termNode.setProperty("XRef", "A001")

      val partOf = new RelationshipType {
        override def name(): String = "PART_OF"
      }

      val encodes = new RelationshipType {
        override def name(): String = "ENCODES"
      }

      val linkTo = new RelationshipType {
        override def name(): String = "LINK_TO"
      }

      val evidence = new RelationshipType {
        override def name(): String = "EVIDENCE"
      }

      geneNode.createRelationshipTo(organismNode, partOf)
      geneNode.createRelationshipTo(polyNode, encodes)
      geneNode.createRelationshipTo(termNode, evidence)
      polyNode.createRelationshipTo(organismNode, partOf)
      termNode.createRelationshipTo(dbNode, linkTo)


      tx.success()
      println("Succesfull transaction.")
    }
    finally {
      tx.close()
      gdb.shutdown()
      println("Succesful disconnect.")
    }
  }
}
val runScript = new myNeo4j
//runScript.main()