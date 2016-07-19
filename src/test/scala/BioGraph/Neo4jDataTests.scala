package BioGraph

import org.neo4j.graphdb.DynamicLabel
import org.scalatest.FunSuite
import org.neo4j.test.TestGraphDatabaseFactory
import utilFunctions.TransactionSupport
import scala.collection.JavaConverters._

/**
  * Created by artem on 29.06.16.
  */
class Neo4jDataTests extends FunSuite with TransactionSupport{

  val graphDataBaseConnection = new TestGraphDatabaseFactory().newImpermanentDatabase()

  def createFirstNodes(): Unit = transaction(graphDataBaseConnection) {
    val testNode = graphDataBaseConnection.createNode()
    testNode.addLabel(DynamicLabel.label("Test Node"))
    testNode.addLabel(DynamicLabel.label("So much very exciting"))
    testNode.setProperty("very", "important")
  }
  createFirstNodes()

  def findNode = transaction(graphDataBaseConnection) {
    val foundNode = graphDataBaseConnection.findNode(DynamicLabel.label("Test Node"), "very", "important")
    foundNode
  }


  test("test find node labels") {
    val foundNode = findNode
    def getLabels = transaction(graphDataBaseConnection) {
      foundNode.getLabels.asScala.toList
    }
    assert(getLabels === List(DynamicLabel.label("Test Node"), DynamicLabel.label("So much very exciting")))
  }

  test("test find node") {
    val foundNode = findNode
    assert((foundNode.getId > -1) === true)
  }

  graphDataBaseConnection.shutdown()

}
