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

  val hasName = new RelationshipType {
    override def name(): String = "HAS_NAME"
  }

  val next = new RelationshipType {
    override def name(): String = "NEXT"
  }

  val overlap = new RelationshipType {
    override def name(): String = "OVERLAP"
  }

  val participates_in = new RelationshipType {
    override def name(): String = "PARTICIPATES_IN"
  }

  val locates_in = new RelationshipType {
    override def name(): String = "LOCATES_IN"
  }
}