package smt.db.impl

import java.sql.{Connection => JConnection, _}
import java.util.Date
import scala.util.control.Exception.{Catcher, catching}
import smt.util.Util
import Util._
import collection.Map.empty
import smt.db.{DatabaseId, MetaConnection, Connection, Database}
import smt.migration.{Script, MigrationInfo, Direction}
import scalaz.{-\/, \/-, \/}
import scala.collection.immutable.Stream.Empty
import sbt.Logger

object SqlDatabase {
  def fromTryCatch[A](block: => A): String \/ A = {
    try {
      \/-(block)
    } catch {
      case e: Exception => -\/(e.toString)
    }
  }
}

object SqlConnection {

  def withStatement[U](c: JConnection)(f: Statement => U, ca: Catcher[U] = empty[Throwable, U]): U = {
    val st = c.createStatement()
    catching(ca).andFinally(st.close())(f(st))
  }

  def withPreparedStatement[U](c: JConnection, sql: String)(f: PreparedStatement => U, ca: Catcher[U] = empty[Throwable, U]) = {
    val st = c.prepareStatement(sql)
    catching(ca).andFinally(st.close())(f(st))
  }

  def withCallableStatement[U](c: JConnection, sql: String)(f: CallableStatement => U, ca: Catcher[U] = empty[Throwable, U]) = {
    val st = c.prepareCall(sql)
    catching(ca).andFinally(st.close())(f(st))
  }

  def mapResultSet[A](rs: ResultSet)(f: ResultSet => A): Stream[A] = {
    if (rs.next) {
      val a = f(rs)
      a #:: mapResultSet(rs)(f)
    }
    else Empty
  }
}

trait CommonSqlConnection {
  commonSqlConnection =>

  import SqlDatabase._
  import SqlConnection._

  val cnx: JConnection

  def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit = fromTryCatch {
    logger.info("applying " + direction + " script: " + script)
    withStatement(cnx)(_.execute(script.content))
  }


  def testScript(logger: Logger)(script: Script): String \/ Unit = fromTryCatch {
    logger.info("applying test script: " + script)
    withStatement(cnx)(_.execute(script.content))
  }

  def close(logger: Logger)(): \/[String, Unit] = fromTryCatch(cnx.close())
}

abstract class SqlConnection(val cnx: JConnection) extends Connection with CommonSqlConnection

abstract class SqlMetaConnection(val cnx: JConnection,
                             tableSchema: Option[String],
                             migrationTableName: String,
                             downTableName: String) extends MetaConnection with CommonSqlConnection {
  sqlConnection =>

  def noDataCatcher[A]: Catcher[Seq[A]]

  val NAME = "NAME"
  val DBID = "DBID"
  val HASH = "HASH"
  val TIME = "TIME"
  val INDEX = "INDX"
  val SCRIPT = "SCRIP"
  val USER = "USR"
  val REMARK = "REMARK"

  val MIGRATION = tableSchema.map(_ + ".").getOrElse("") + migrationTableName
  val DOWN = tableSchema.map(_ + ".").getOrElse("") + downTableName

  val createMigrationTableString = "CREATE TABLE " + MIGRATION + "( " + INDEX + " NUMBER(10), " + NAME + " VARCHAR(128), " +
    HASH + " VARCHAR(40), " + TIME + " NUMBER(15), " + USER + " VARCHAR(100), " + REMARK + " VARCHAR(256) )"

  val createDownTableString = "CREATE TABLE " + DOWN + "( " + HASH + " VARCHAR(40), " + INDEX + " NUMBER(10), " +
    SCRIPT + " CLOB, " + NAME + " VARCHAR(128) )"

  val queryMigrationTableString = "SELECT * FROM " + MIGRATION

  def insertMigrationString(mi: MigrationInfo, index: Long): String = {
    val cols = Seq(Some(INDEX), Some(NAME), Some(DBID), Some(HASH), Some(TIME), mi.user.map(_ => USER), mi.remark.map(_ => REMARK)).flatten
    val vals = Seq(Some(index.toString), Some(" '" + mi.name + "' "), Some(" '" + mi.dbId.id + "' "), Some(" '" + bytesToHex(mi.hash) + "' "), Some(mi.dateTime.getTime.toString), mi.user.map(" '" + _ + "' "), mi.remark.map(" '" + _ + "' ")).flatten

    "INSERT INTO " + MIGRATION + "( " + cols.mkString(", ") + " )" +
      " VALUES ( " + vals.mkString(", ") + " )"
  }

  def removeMigrationString(hash: Seq[Byte]): String = {
    "DELETE FROM " + MIGRATION + " WHERE HASH = '" + bytesToHex(hash) + "'"
  }

  def removeDownsString(hash: Seq[Byte]): String = {
    "DELETE FROM " + DOWN + " WHERE HASH = '" + bytesToHex(hash) + "'"
  }

  def insertDownString(name: String, hash: Seq[Byte], index: Int) = "INSERT INTO " + DOWN + " VALUES ('" + bytesToHex(hash) + "', " + index + ", ?, '" + name + "')"

  def queryDownString(hash: Seq[Byte]) = "SELECT * FROM " + DOWN + " WHERE HASH = '" + bytesToHex(hash) + "'"

  import SqlDatabase._
  import SqlConnection._

  def createMigrationTable() = withStatement(cnx)(_.execute(createMigrationTableString))

  def createDownTable() = withStatement(cnx)(_.execute(createDownTableString))

  def isResultSizeOne(selectString: String): Boolean = {
    val tn = withStatement(cnx)(st => {
      mapResultSet(st.executeQuery(selectString))(rs => ()).toList
    }, noDataCatcher)
    tn.size == 1
  }

  def queryTableExistsString(table: String): String

  def queryMigrationTableHasColumnString(column: String): String

  def doesMigrationTableExist(): Boolean = isResultSizeOne(queryTableExistsString(migrationTableName))

  def doesMigrationTableHaveUserColumn(): Boolean = isResultSizeOne(queryMigrationTableHasColumnString(USER))

  def doesMigrationTableHaveRemarkColumn(): Boolean = isResultSizeOne(queryMigrationTableHasColumnString(REMARK))

  def alterMigrationTableAddColumnString(column: String) = "ALTER TABLE " + MIGRATION + " ADD ( " + column + ")"

  def alterMigrationTableAddUserColumn() = withStatement(cnx)(_.execute(alterMigrationTableAddColumnString(USER + " VARCHAR(100)")))

  def alterMigrationTableAddRemarkColumn() = withStatement(cnx)(_.execute(alterMigrationTableAddColumnString(REMARK + " VARCHAR(256)")))

  def doesDownTableExist(): Boolean = isResultSizeOne(queryTableExistsString(downTableName))

  def init(logger: Logger)(): String \/ Unit = fromTryCatch {
    if (doesMigrationTableExist()) {
      if (!doesMigrationTableHaveUserColumn()) alterMigrationTableAddUserColumn()
      if (!doesMigrationTableHaveRemarkColumn()) alterMigrationTableAddRemarkColumn()
    }
    else createMigrationTable()

    if (!doesDownTableExist()) createDownTable()
  }

  def acquireLock(logger: Logger)(): \/[String, String] = {
    logger.info("Acquiring lock")
    \/-("")
  }

  def releaseLock(logger: Logger)(lock: String): \/[String, Unit] = {
    logger.info("Releasing lock")
    \/-()
  }

  def add(logger: Logger)(migrationInfo: MigrationInfo): String \/ Unit = fromTryCatch {
    val mm = withStatement(cnx)(st => {
      mapResultSet(st.executeQuery(queryMigrationTableString))(rs => {
        rs.getLong(INDEX)
      }).toSeq.sorted(Ordering[Long].reverse).headOption
    }, {
      case e: SQLException => None
    })

    val mi = mm.getOrElse(0L) + 1L
    logger.info("adding migration " + mi + ", " + migrationInfo)

    withStatement(cnx)(_.execute(insertMigrationString(migrationInfo, mi)))
  }

  def addDowns(logger: Logger)(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = fromTryCatch {
    logger.info("adding " + downs.size + " downs")
    def addDown(i: Int, down: Script) {
      logger.info("adding down: " + down)
      val clob = cnx.createClob()
      clob.setString(1, down.content)
      withPreparedStatement(cnx, insertDownString(down.name, migHash, i))(st => {
        st.setClob(1, clob)
        st.executeUpdate()
      })
    }

    downs.zipWithIndex.foreach(t => addDown(t._2, t._1))
  }

  def remove(logger: Logger)(hash: Seq[Byte]): String \/ Unit = fromTryCatch {
    logger.info("removing " + bytesToHex(hash))
    withStatement(cnx)(_.execute(removeMigrationString(hash)))
  }

  def removeDowns(logger: Logger)(migHash: Seq[Byte]): String \/ Unit = fromTryCatch {
    logger.info("removing downs for " + bytesToHex(migHash))
    withStatement(cnx)(_.execute(removeDownsString(migHash)))
  }

  def state(logger: Logger): String \/ Seq[MigrationInfo] = fromTryCatch(withStatement(cnx)(st => {
    mapResultSet(st.executeQuery(queryMigrationTableString))(rs => {
      (MigrationInfo(
        name = rs.getString(NAME),
        dbId = DatabaseId(rs.getString(DBID)),
        hash = hexToBytes(rs.getString(HASH)),
        dateTime = new Date(rs.getLong(TIME)),
        user = Option(rs.getString(USER)),
        remark = Option(rs.getString(REMARK))
      ),
        rs.getLong(INDEX)
        )
    }).toSeq.sortBy(_._2).map(_._1)
  }, noDataCatcher))

  def downs(logger: Logger)(hash: Seq[Byte]): String \/ Seq[Script] = fromTryCatch(withStatement(cnx)(st => {
    mapResultSet(st.executeQuery(queryDownString(hash)))(rs => {
      val index = rs.getInt(INDEX)
      val name = Option(rs.getString(NAME)).getOrElse(index.toString)
      val clob = rs.getClob(SCRIPT)
      val down = Script(name = name, content = clob.getSubString(1, clob.length().toInt))
      (down, index)
    }).toSeq.sortBy(_._2).map(_._1)
  }, noDataCatcher))
}
