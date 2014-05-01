package smt

import smt.db.Database
import smt.migration.{Direction, Script, MigrationInfo}

class DatabaseMock extends Database {

  var addCount: Int = 0

  def state: Either[Failure, Seq[MigrationInfo]] = Right(Seq())

  def downs(hash: Seq[Byte]): Either[Failure, Seq[Script]] = Right(Seq())

  def add(migrationInfo: MigrationInfo): (Option[Failure], Database) = {
    addCount = addCount + 1
    (None, this)
  }

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database) = (None, this)

  def remove(hash: Seq[Byte]): (Option[Failure], Database) = (None, this)

  def removeDowns(migHash: Seq[Byte]): (Option[Failure], Database) = (None, this)

  def applyScript(script: Script, direction: Direction): (Option[Failure], Database) = (None, this)

  def testScript(script: Script): (Option[Failure], Database) = (None, this)
}

