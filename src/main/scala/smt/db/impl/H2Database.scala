package smt.db.impl

import java.sql.{Connection => JConnection, _}
import scala.util.control.Exception._
import SqlConnection._

class H2Database(connection: => JConnection) extends SqlDatabase(
  new H2Connection(connection)
)

class H2Connection(connection: JConnection) extends SqlConnection(connection) {
  def tableExistsCatcher(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 42101 && e.getMessage.contains(("Table \"" + name + "\" already exists")) => {
      println("Ignoring SqlExecption " + e.getErrorCode + ", " + e.getMessage)
    }
  }

  def noDataCatcher[A]: Catcher[Seq[A]] = {
    case e: SQLException if e.getErrorCode == 2000 => Seq()
  }

  def queryTableExistsString(table: String) = "select * from INFORMATION_SCHEMA.TABLES where TABLE_NAME = '" + table + "'"

  def queryMigrationTableHasColumnString(column: String) = "select * from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '" + MIGRATION + "' " +
    " AND COLUMN_NAME = '" + column + "'"


}
