package smt.db.impl

import java.sql.{Connection => JConnection, _}
import scala.util.control.Exception._
import scala.collection.Map._
import sbt.Logger

class OracleDatabase(connection: => JConnection,
                     tableSchema: Option[String] = None,
                     migrationTableName: String = "MIGRATION",
                     downTableName: String = "DOWN") extends SqlDatabase(
  new OracleConnection(connection, tableSchema, migrationTableName, downTableName)
)

class OracleConnection(connection: JConnection,
                       tableSchema: Option[String],
                       migrationTableName: String,
                       downTableName: String) extends SqlConnection(connection, tableSchema, migrationTableName, downTableName) {
  def tableExistsCatcher(logger: Logger)(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 955 => {
      logger.info("Ignoring SqlExecption " + e.getErrorCode + ", " + e.getMessage)
    }
  }

  def noDataCatcher[A]: Catcher[Seq[A]] = empty

  def queryTableExistsString(table: String): String = tableSchema match {
    case None => "select TNAME from SYS.TAB where TNAME = '" + table + "'"
    case Some(schema) => "select TABLE_NAME from DBA_TABLES where TABLE_NAME = '" + table + "' and OWNER = '" + schema + "'"
  }

  def queryMigrationTableHasColumnString(column: String): String = tableSchema match {
    case None => "select CNAME from SYS.COL where TNAME = '" + migrationTableName + "' and CNAME = '" + column + "'"
    case Some(schema) => "select COLUMN_NAME from DBA_TAB_COLS where TABLE_NAME = '" + migrationTableName + "' and OWNER = '" + schema + "'"
  }
}
