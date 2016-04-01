package smt

import smt.db.{MetaDatabase, MetaConnection}
import scalaz.{\/-, \/}

class MetaDatabaseMock(metaConnectionMock: MetaConnection) extends MetaDatabase {
  def connection(): \/[String, MetaConnection] = \/-(metaConnectionMock)
}
