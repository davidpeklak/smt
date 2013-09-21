package smt

trait Database {

  def state: Seq[MigrationInfo]

  def downs(hash: Seq[Byte]): Seq[Script]

  def add(migrationInfo: MigrationInfo): Either[String, Database]

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): Either[String, Database]

  def remove(hash: Seq[Byte]): Either[String, Database]

  def removeDowns(migHash: Seq[Byte]): Either[String, Database]

  def apply(script: Script): Either[String, Database]
}