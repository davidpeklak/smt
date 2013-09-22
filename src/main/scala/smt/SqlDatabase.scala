package smt

import java.sql.{Connection => JConnection, _}
import java.util.Date
import util.control.Exception.{Catcher, catching, allCatcher}
import Util._
import collection.Map.empty

abstract class SqlDatabase(connection: => JConnection) extends Database {
  sqlDatabase =>

  def tableExistsCatcher(name: String): Catcher[Unit]

  def noDataCatcher[A]: Catcher[Seq[A]]

  private def exceptionToFailure[A](block: => A): Either[Failure, A] = {
    catching(allCatcher[A]).either(block).left.map(_.toString)
  }

  private def effectExceptionToFailure(block: => Unit): (Option[Failure], Database) = {
    (exceptionToFailure(block).left.toOption, this)
  }

  def withStatement[U](c: JConnection)(f: Statement => U, ca: Catcher[U] = empty[Throwable, U]): U = {
    val st = c.createStatement()
    catching(ca).andFinally(st.close())(f(st))
  }

  def withPreparedStatement[U](c: JConnection, sql: String)(f: PreparedStatement => U, ca: Catcher[U] = empty[Throwable, U]) = {
    val st = c.prepareStatement(sql)
    catching(ca).andFinally(st.close())(f(st))
  }

  val NAME = "NAME"
  val HASH = "HASH"
  val TIME = "TIME"
  val INDEX = "INDX"
  val MIGRATION = "MIGRATION"
  val DOWN = "DOWN"
  val SCRIPT = "SCRIP"

  val createMigrationTableString = "CREATE TABLE " + MIGRATION + "( " + INDEX + " NUMBER(10), " + NAME + " VARCHAR(128), " +
    HASH + " VARCHAR(40), " + TIME + " NUMBER(15) )"

  val createDownsTableString = "CREATE TABLE " + DOWN + "( " + HASH + " VARCHAR(40), " + INDEX + " NUMBER(10), " +
    SCRIPT + " CLOB )"

  val queryMigrationTableString = "SELECT * FROM " + MIGRATION

  def insertMigrationString(mi: MigrationInfo, index: Long): String = {
    "INSERT INTO " + MIGRATION + " VALUES ( " + index + ", '" + mi.name + "', '" + bytesToHex(mi.hash) + "', " + mi.dateTime.getTime + " )"
  }

  def removeMigrationString(hash: Seq[Byte]): String = {
    "DELETE FROM " + MIGRATION + " WHERE HASH = '" + bytesToHex(hash) + "'"
  }

  def removeDownsString(hash: Seq[Byte]): String = {
    "DELETE FROM " + DOWN + " WHERE HASH = '" + bytesToHex(hash) + "'"
  }

  def insertDownString(hash: Seq[Byte], index: Int) = "INSERT INTO " + DOWN + " VALUES ('" + bytesToHex(hash) + "', " + index + ", ?)"

  def queryDownString(hash: Seq[Byte]) = "SELECT * FROM " + DOWN + " WHERE HASH = '" + bytesToHex(hash) + "'"

  lazy val cnx = {
    val cnx = connection
    withStatement(cnx)(_.execute(createMigrationTableString), tableExistsCatcher(MIGRATION))
    withStatement(cnx)(_.execute(createDownsTableString), tableExistsCatcher(DOWN))
    cnx
  }

  def add(migrationInfo: MigrationInfo): (Option[Failure], Database) = effectExceptionToFailure {
    val mm = withStatement(cnx)(st => {
      mapResultSet(st.executeQuery(queryMigrationTableString))(rs => {
        rs.getLong(INDEX)
      }).toSeq.sorted(Ordering[Long].reverse).headOption
    }, {
      case e: SQLException => None
    })

    val mi = mm.getOrElse(0L) + 1L
    println("adding migration " + mi + ", " + migrationInfo)

    withStatement(cnx)(_.execute(insertMigrationString(migrationInfo, mi)))
  }

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database) = effectExceptionToFailure {
    println("adding " + downs.size + " downs")
    def addDown(i: Int, down: Script) {
      println("adding down: " + down)
      val clob = cnx.createClob()
      clob.setString(1, down.content)
      withPreparedStatement(cnx, insertDownString(migHash, i))(st => {
        st.setClob(1, clob)
        st.executeUpdate()
      })
    }

    downs.zipWithIndex.foreach(t => addDown(t._2, t._1))
  }

  def remove(hash: Seq[Byte]): (Option[Failure], Database) = effectExceptionToFailure {
    println("removing " + bytesToHex(hash))
    withStatement(cnx)(_.execute(removeMigrationString(hash)))
  }

  def removeDowns(migHash: Seq[Byte]): (Option[Failure], Database) = effectExceptionToFailure {
    println("removing downs for " + bytesToHex(migHash))
    withStatement(cnx)(_.execute(removeDownsString(migHash)))
  }

  def applyScript(script: Script): (Option[Failure], Database) = effectExceptionToFailure {
    println("applying script: " + script)
    withStatement(cnx)(_.execute(script.content))
  }

  def state: Either[Failure, Seq[MigrationInfo]] = exceptionToFailure(withStatement(cnx)(st => {
    mapResultSet(st.executeQuery(queryMigrationTableString))(rs => {
      (MigrationInfo(
        name = rs.getString(NAME),
        hash = hexToBytes(rs.getString(HASH)),
        dateTime = new Date(rs.getLong(TIME))
      ),
        rs.getLong(INDEX)
        )
    }).toSeq.sortBy(_._2).map(_._1)
  }, noDataCatcher))

  def downs(hash: Seq[Byte]): Either[Failure, Seq[Script]] = exceptionToFailure(withStatement(cnx)(st => {
    mapResultSet(st.executeQuery(queryDownString(hash)))(rs => {
      val index = rs.getInt(INDEX)
      val clob = rs.getClob(SCRIPT)
      val down = Script(name = index.toString, content = clob.getSubString(1, clob.length().toInt))
      (down, index)
    }).toSeq.sortBy(_._2).map(_._1)
  }, noDataCatcher))
}
