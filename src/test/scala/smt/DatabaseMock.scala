package smt

import smt.db.{Database, Connection}
import scalaz.{\/-, \/}

class DatabaseMock(connectionMock: Connection) extends Database {
  def connection(): \/[String, Connection] = \/-(connectionMock)
}
