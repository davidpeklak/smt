package smt

import smt.db.Connection
import smt.migration.{Direction, Script, MigrationInfo}
import scalaz.{\/-, \/}

class ConnectionMock extends Connection {

  var addCount: Int = 0

  def init(): String \/ Unit = \/-(())

  def state: String \/ Seq[MigrationInfo] = \/-(Seq())

  def downs(hash: Seq[Byte]): String \/ Seq[Script] = \/-(Seq())

  def add(migrationInfo: MigrationInfo): String \/ Unit = {
    addCount = addCount + 1
    \/-(())
  }

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = \/-(())

  def remove(hash: Seq[Byte]): String \/ Unit = \/-(())

  def removeDowns(migHash: Seq[Byte]): String \/ Unit = \/-(())

  def applyScript(script: Script, direction: Direction): String \/ Unit = \/-(())

  def testScript(script: Script): String \/ Unit = \/-(())

  def close(): String \/ Unit = \/-(())
}

