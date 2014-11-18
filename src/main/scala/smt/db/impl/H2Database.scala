package smt.db.impl

import java.sql.{Connection => JConnection, _}
import scala.util.control.Exception._
import sbt.Logger
import SqlConnection._

class H2Database(connection: => JConnection,
                 tableSchema: Option[String] = None,
                 migrationTableName: String = "MIGRATION",
                 downTableName: String = "DOWN") extends SqlDatabase(
  new H2Connection(connection, tableSchema, migrationTableName, downTableName)
)

class H2Connection(connection: JConnection,
                   tableSchema: Option[String],
                   migrationTableName: String,
                   downTableName: String) extends SqlConnection(connection, tableSchema, migrationTableName, downTableName) {
  def tableExistsCatcher(logger: Logger)(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 42101 && e.getMessage.contains(("Table \"" + name + "\" already exists")) => {
      logger.info("Ignoring SqlExecption " + e.getErrorCode + ", " + e.getMessage)
    }
  }

  def noDataCatcher[A]: Catcher[Seq[A]] = {
    case e: SQLException if e.getErrorCode == 2000 => Seq()
  }

  def queryTableExistsString(table: String) = "select * from INFORMATION_SCHEMA.TABLES where TABLE_NAME = '" + table + "'"

  def queryMigrationTableHasColumnString(column: String) = "select * from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '" + MIGRATION + "' " +
    " AND COLUMN_NAME = '" + column + "'"


}
