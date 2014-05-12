package smt.db.impl

import smt.util.Util
import Util._
import smt.db.{Connection, Database}
import smt.migration.{Script, MigrationInfo, Direction}
import scalaz.{\/-, \/}

class TestDatabase extends Database {
  def connection(): \/[String, Connection] = {
    \/-(new TestConnection)
  }
}

class TestConnection extends Connection {

  private var s: Seq[MigrationInfo] = Nil

  private var ds: Map[Seq[Byte], Seq[Script]] = Map()

  def init(): String \/ Unit = {
    \/-()
  }

  def add(migrationInfo: MigrationInfo): String \/ Unit = {
    println("adding " + migrationInfo)
    s = s :+ migrationInfo
    \/-()
  }

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = {
    println("adding " + downs)
    ds = ds + (migHash -> downs)
    \/-()
  }

  def remove(hash: Seq[Byte]): String \/ Unit = {
    println("removing " + bytesToHex(hash))
    s = s.filterNot(_.hash == hash)
    \/-()
  }

  def removeDowns(migHash: Seq[Byte]): String \/ Unit = {
    println("removing downs" + bytesToHex(migHash))
    ds = ds - migHash
    \/-()
  }

  def applyScript(script: Script, direction: Direction): String \/ Unit = {
    println("applying " + direction + " " + script)
    \/-()
  }


  def testScript(script: Script): String \/ Unit = {
    println("applying test " + script)
    \/-()
  }

  def state: String \/ Seq[MigrationInfo] = \/-(s)

  def downs(hash: Seq[Byte]): String \/ Seq[Script] = \/-(ds(hash))

  def close(): String \/ Unit = {
    println("closing")
    \/-()
  }
}
