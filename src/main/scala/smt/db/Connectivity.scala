package smt.db

case class Connectivity(metadataConnection: Connection, databases: Map[DatabaseId, Database])
