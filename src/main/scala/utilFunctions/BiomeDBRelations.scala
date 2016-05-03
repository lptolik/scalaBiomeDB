package utilFunctions

import org.neo4j.graphdb.{Label, RelationshipType}

/**
  * Created by artem on 14.03.16.
  */
object BiomeDBRelations {
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

  val similar = new RelationshipType {
    override def name(): String = "SIMILAR"
  }

  val isA = new RelationshipType {
    override def name(): String = "IS_A"
  }
}