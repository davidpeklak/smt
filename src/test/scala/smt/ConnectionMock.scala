package smt

import smt.db.Connection
import smt.migration.{Direction, Script, MigrationInfo}
import scalaz.{\/-, \/}
import sbt.{Level, Logger}

class ConnectionMock extends Connection {

  var addCount: Int = 0

  def init(logger: Logger)(): String \/ Unit = \/-(())

  def state(logger: Logger): String \/ Seq[MigrationInfo] = \/-(Seq())

  def downs(logger: Logger)(hash: Seq[Byte]): String \/ Seq[Script] = \/-(Seq())

  def add(logger: Logger)(migrationInfo: MigrationInfo): String \/ Unit = {
    addCount = addCount + 1
    \/-(())
  }

  def addDowns(logger: Logger)(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = \/-(())

  def remove(logger: Logger)(hash: Seq[Byte]): String \/ Unit = \/-(())

  def removeDowns(logger: Logger)(migHash: Seq[Byte]): String \/ Unit = \/-(())

  def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit = \/-(())

  def testScript(logger: Logger)(script: Script): String \/ Unit = \/-(())

  def close(logger: Logger)(): String \/ Unit = \/-(())

  def acquireLock(logger: Logger)(): String \/ String  = \/-("lock")

  def releaseLock(logger: Logger)(lock: String): String \/ Unit = \/-(())
}

