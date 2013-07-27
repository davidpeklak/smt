package smt

import java.sql.{Connection => JConnection, _}
import scala.util.control.Exception._
import scala.collection.Map._

class OracleDatabase(connection: => JConnection) extends SqlDatabase(connection) {

  def tableExistsCatcher(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 955 => {
      println("Ignoring SqlExecption " + e.getErrorCode + ", " + e.getMessage)
    }
  }

  def noDataCatcher[A]: Catcher[Seq[A]] = empty
}
