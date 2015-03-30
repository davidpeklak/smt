package smt.migration

case class HashedMigrationSeq (initMig: Int, migs: Seq[(Migration, Seq[Byte])])
