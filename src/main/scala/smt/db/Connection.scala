package smt.db

import smt.migration.{Direction, Script}
import scalaz.\/
import smt.util.Logger

trait Connection {
  def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit

  def testScript(logger: Logger)(script: Script): String \/ Unit

  def close(logger: Logger)(): String \/ Unit
}