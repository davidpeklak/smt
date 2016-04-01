package smt.db

import scalaz.\/

trait MetaDatabase {
  def connection(): String \/ MetaConnection
}
