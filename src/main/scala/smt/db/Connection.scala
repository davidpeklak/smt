package smt.db

import smt.migration.{Direction, Script, MigrationInfo}
import scalaz.\/

trait Connection {

  def init(): String \/ Unit
  
  def state: String \/ Seq[MigrationInfo]

  def downs(hash: Seq[Byte]): String \/ Seq[Script]

  def add(migrationInfo: MigrationInfo): String \/ Unit

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit

  def remove(hash: Seq[Byte]): String \/ Unit

  def removeDowns(migHash: Seq[Byte]): String \/ Unit

  def applyScript(script: Script, direction: Direction): String \/ Unit

  def testScript(script: Script): String \/ Unit

  def close(): String \/ Unit
}
