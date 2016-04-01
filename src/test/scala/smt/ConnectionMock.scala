package smt

import smt.db.Connection
import smt.migration.{Direction, Script, MigrationInfo}
import scalaz.{\/-, \/}
import sbt.{Level, Logger}

class ConnectionMock extends Connection {

  def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit = \/-(())

  def testScript(logger: Logger)(script: Script): String \/ Unit = \/-(())

  def close(logger: Logger)(): String \/ Unit = \/-(())
}

