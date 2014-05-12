package smt.db.impl

import java.sql.{Connection => JConnection, _}
import scala.util.control.Exception._
import scala.collection.Map._

class OracleDatabase(connection: => JConnection) extends SqlDatabase(
  new OracleConnection(connection)
)

class OracleConnection(connection: JConnection) extends SqlConnection(connection) {
  def tableExistsCatcher(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 955 => {
      println("Ignoring SqlExecption " + e.getErrorCode + ", " + e.getMessage)
    }
  }

  def noDataCatcher[A]: Catcher[Seq[A]] = empty
}
