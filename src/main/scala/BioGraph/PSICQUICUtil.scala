package BioGraph

import java.io.File

import org.hupo.psi.mi.psicquic.wsclient.UniversalPsicquicClient
import org.hupo.psi.mi.psicquic.wsclient.result.MitabSearchResult
import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import psidev.psi.mi.tab.converter.tab2xml.Tab2Xml
import psidev.psi.mi.xml.model.EntrySet
import utilFunctions.BiomeDBRelations

import scala.collection.JavaConverters._

/**
  * Created by artem on 06.07.16.
  */
case class PSICQUICUtil(serverAddress: String) {
  val client = new UniversalPsicquicClient(serverAddress)

  def sendQuery(query: String, firstResult: Int = 0, maxResult: Int = 200): MitabSearchResult = {
    val queryResult = client.getByQuery(query, firstResult, maxResult)
    queryResult
  }

  def convertQueryResult(queryResult: MitabSearchResult): EntrySet = {
    val xmlConverter = new Tab2Xml
    val psiXmlResult = xmlConverter.convert(queryResult.getData)
    psiXmlResult
//    psiXmlResult.getEntries.asScala
  }

  def searchForReactant(queryXmlResult: EntrySet, dataBaseFile: File) = {
    val graphDataBaseConnection = new GraphDatabaseFactory().newEmbeddedDatabase(dataBaseFile)
    val entries = queryXmlResult.getEntries.asScala
    val reader = new IntactUtil(entries)
    val reactants = reader.getInteractors.map(reader.interactorInfo)
    val reactionInfo = reader.getInteractions.map(reader.interactionInfo)
    val xrefNode = graphDataBaseConnection.findNode(DynamicLabel.label("XRef"), "id", reactionInfo.head.getIntactId)
    val reactionNode = xrefNode.getRelationships(BiomeDBRelations.evidence).asScala.head.getStartNode
  }
}
