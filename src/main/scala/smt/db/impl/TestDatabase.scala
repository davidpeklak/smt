package smt.db.impl

import smt.util.Util
import Util._
import smt.db.{Connection, Database}
import smt.migration.{Script, MigrationInfo, Direction}
import scalaz.{\/-, \/}
import sbt.Logger

class TestDatabase extends Database {
  def connection(): \/[String, Connection] = {
    \/-(new TestConnection)
  }
}

class TestConnection extends Connection {

  private var s: Seq[MigrationInfo] = Nil

  private var ds: Map[Seq[Byte], Seq[Script]] = Map()

  def init(logger: Logger)(): String \/ Unit = {
    logger.info("initializing ")
    \/-()
  }

  def acquireLock(logger: Logger)(): \/[String, String] = \/-("")

  def releaseLock(logger: Logger)(lock: String): \/[String, Unit] = \/-()

  def add(logger: Logger)(migrationInfo: MigrationInfo): String \/ Unit = {
    logger.info("adding " + migrationInfo)
    s = s :+ migrationInfo
    \/-()
  }

  def addDowns(logger: Logger)(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = {
    logger.info("adding " + downs)
    ds = ds + (migHash -> downs)
    \/-()
  }

  def remove(logger: Logger)(hash: Seq[Byte]): String \/ Unit = {
    logger.info("removing " + bytesToHex(hash))
    s = s.filterNot(_.hash == hash)
    \/-()
  }

  def removeDowns(logger: Logger)(migHash: Seq[Byte]): String \/ Unit = {
    logger.info("removing downs" + bytesToHex(migHash))
    ds = ds - migHash
    \/-()
  }

  def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit = {
    logger.info("applying " + direction + " " + script)
    \/-()
  }


  def testScript(logger: Logger)(script: Script): String \/ Unit = {
    logger.info("applying test " + script)
    \/-()
  }

  def state(logger: Logger): String \/ Seq[MigrationInfo] = \/-(s)

  def downs(logger: Logger)(hash: Seq[Byte]): String \/ Seq[Script] = \/-(ds(hash))

  def close(logger: Logger)(): String \/ Unit = {
    logger.info("closing")
    \/-()
  }
}
