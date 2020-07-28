package BioGraph

import org.neo4j.graphdb.{GraphDatabaseService, Label}
import org.scalatest.FunSuite
import org.neo4j.test.TestGraphDatabaseFactory
import utilFunctions.TransactionSupport
import scala.collection.JavaConverters._

/**
  * Created by artem on 29.06.16.
  */
class Neo4jDataTests extends FunSuite with TransactionSupport{

  def createFirstNodes(graphDataBaseConnection: GraphDatabaseService): Unit = transaction(graphDataBaseConnection) {
    val testNode = graphDataBaseConnection.createNode()
    testNode.addLabel(Label.label("Test Node"))
    testNode.addLabel(Label.label("So much very exciting"))
    testNode.setProperty("very", "important")
  }

  def findNode(graphDataBaseConnection: GraphDatabaseService) = transaction(graphDataBaseConnection) {
    val foundNode = graphDataBaseConnection.findNode(Label.label("Test Node"), "very", "important")
    foundNode
  }

  test("test find node labels") {
    val graphDataBaseConnection = new TestGraphDatabaseFactory().newImpermanentDatabase()
    createFirstNodes(graphDataBaseConnection)
    val foundNode = findNode(graphDataBaseConnection)
    def getLabels = transaction(graphDataBaseConnection) {
      foundNode.getLabels.asScala.toList
    }
    assert(getLabels === List(Label.label("Test Node"), Label.label("So much very exciting")))
    graphDataBaseConnection.shutdown()
  }

  test("test find node") {
    val graphDataBaseConnection = new TestGraphDatabaseFactory().newImpermanentDatabase()
    createFirstNodes(graphDataBaseConnection)
    val foundNode = findNode(graphDataBaseConnection)
    assert((foundNode.getId > -1) === true)
    graphDataBaseConnection.shutdown()
  }

  test("checkChanginNodeId") {
    val graphDataBaseConnection = new TestGraphDatabaseFactory().newImpermanentDatabase()
    def createTaxon(graphDataBaseConnection: GraphDatabaseService) = transaction(graphDataBaseConnection) {
      val testTaxon = Taxon("Taxon", TaxonType.no_rank)
      testTaxon.upload(graphDataBaseConnection)
      val testTaxon2 = Taxon("Taxon2", TaxonType.no_rank)
      testTaxon2.upload(graphDataBaseConnection)
      testTaxon2
    }
    val testTaxon = createTaxon(graphDataBaseConnection)
    assert((testTaxon.getId > -1) === true)
    graphDataBaseConnection.shutdown()
  }

}
