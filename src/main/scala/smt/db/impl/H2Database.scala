package smt.db.impl

import java.sql.{Connection => JConnection, _}
import scala.util.control.Exception._

class H2Database(connection: => JConnection) extends SqlDatabase(connection) {

  def tableExistsCatcher(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 42101 && e.getMessage.contains(("Table \"" + name + "\" already exists")) => {
      println("Ignoring SqlExecption " + e.getErrorCode + ", " + e.getMessage)
    }
  }

  def noDataCatcher[A]: Catcher[Seq[A]] =  {
    case e: SQLException if e.getErrorCode == 2000 => Seq()
  }
}
