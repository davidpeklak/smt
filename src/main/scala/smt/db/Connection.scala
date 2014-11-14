package smt.db

import smt.migration.{Direction, Script, MigrationInfo}
import scalaz.\/
import sbt.Logger

trait Connection {

  def init(logger: Logger)(): String \/ Unit
  
  def state(logger: Logger): String \/ Seq[MigrationInfo]

  def downs(logger: Logger)(hash: Seq[Byte]): String \/ Seq[Script]

  def add(logger: Logger)(migrationInfo: MigrationInfo): String \/ Unit

  def addDowns(logger: Logger)(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit

  def remove(logger: Logger)(hash: Seq[Byte]): String \/ Unit

  def removeDowns(logger: Logger)(migHash: Seq[Byte]): String \/ Unit

  def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit

  def testScript(logger: Logger)(script: Script): String \/ Unit

  def close(logger: Logger)(): String \/ Unit
}
