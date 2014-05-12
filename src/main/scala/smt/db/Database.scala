package smt.db

import scalaz.\/

trait Database {
  def connection(): String \/ Connection
}
