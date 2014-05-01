package smt.db

import smt.migration.{Script, MigrationInfo, Direction}

trait Database {

  type Failure = String

  def state: Either[Failure, Seq[MigrationInfo]]

  def downs(hash: Seq[Byte]): Either[Failure, Seq[Script]]

  def add(migrationInfo: MigrationInfo): (Option[Failure], Database)

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database)

  def remove(hash: Seq[Byte]): (Option[Failure], Database)

  def removeDowns(migHash: Seq[Byte]): (Option[Failure], Database)

  def applyScript(script: Script, direction: Direction): (Option[Failure], Database)

  def testScript(script: Script): (Option[Failure], Database)
}
