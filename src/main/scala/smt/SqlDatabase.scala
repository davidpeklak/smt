package smt

import java.sql.{Connection => JConnection, _}
import java.util.Date
import util.control.Exception.{Catcher, catching, allCatch, allCatcher}
import Util._
import collection.Map.empty

class SqlDatabase(connectIdentifier: String, driverClass: Class[_]) extends Database {
  sqlDatabase =>

  def withStatement[U](c: JConnection)(f: Statement => U, ca: Catcher[U] = empty[Throwable, U]) = {
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

  def tableExistsCatcher(name: String): Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 42101 && e.getMessage.contains(("Table \"" + name + "\" already exists")) => ()
  }

  def noDataCatcher[A]: Catcher[Seq[A]] = {
    case e: SQLException if e.getErrorCode == 2000 => Seq()
  }

  lazy val connection = {
    val connection = DriverManager.getConnection(connectIdentifier)
    withStatement(connection)(_.execute(createMigrationTableString), tableExistsCatcher(MIGRATION))
    withStatement(connection)(_.execute(createDownsTableString), tableExistsCatcher(DOWN))
    connection
  }

  class SqlTransaction extends Transaction {
    type T = SqlTransaction
    type DB = SqlDatabase

    private def exceptionToEither(block: => Unit): Either[String, SqlTransaction] = {
      val catcher = allCatcher[Unit] andThen ( _ => {
        connection.rollback()
        connection.setAutoCommit(true)
      })
      catching(catcher).either(block).left.map(_.getMessage).right.map(_ => this)
    }

    def add(migrationInfo: MigrationInfo): Either[String, SqlTransaction] = exceptionToEither {
      val mm = withStatement(connection)(st => {
        ResultSetIterator(st.executeQuery(queryMigrationTableString)).map(rs => {
          rs.getLong(INDEX)
        }).toSeq.sorted.headOption
      }, {
        case e: SQLException => None
      })

      val mi = mm.getOrElse(0L) + 1L
      println("adding migration " + mi + ", " + migrationInfo)

      withStatement(connection)(_.execute(insertMigrationString(migrationInfo, mi)))
    }

    def addDowns(migHash: Seq[Byte], downs: Seq[String]): Either[String, SqlTransaction] = exceptionToEither {
      println("adding " + downs.size + " downs")
      def addDown(i: Int, down: String) {
        println("adding down: ")
        println(down)
        val clob = connection.createClob()
        clob.setString(1, down)
        withPreparedStatement(connection, insertDownString(migHash, i))(st => {
          st.setClob(1, clob)
          st.executeUpdate()
        })
      }

      downs.zipWithIndex.foreach(t => addDown(t._2, t._1))
    }

    def remove(hash: Seq[Byte]): Either[String, SqlTransaction] = exceptionToEither {
      println("removing " + bytesToHex(hash))
      withStatement(connection)(_.execute(removeMigrationString(hash)))
    }

    def removeDowns(migHash: Seq[Byte]): Either[String, SqlTransaction] = exceptionToEither {
      println("removing downs for " + bytesToHex(migHash))
      withStatement(connection)(_.execute(removeDownsString(migHash)))
    }

    def apply(script: String): Either[String, SqlTransaction] = exceptionToEither {
      println("applying script: ")
      println(script)
      withStatement(connection)(_.execute(script))
    }

    def commit: SqlDatabase = {
      connection.commit()
      connection.setAutoCommit(true)
      sqlDatabase
    }
  }

  type T = SqlTransaction

  def state: Seq[MigrationInfo] = withStatement(connection)(st => {
    ResultSetIterator(st.executeQuery(queryMigrationTableString)).map(rs => {
      (MigrationInfo(
        name = rs.getString(NAME),
        hash = hexToBytes(rs.getString(HASH)),
        dateTime = new Date(rs.getLong(TIME))
      ),
        rs.getLong(INDEX)
        )
    }).toSeq.sortBy(_._2).map(_._1)
  }, noDataCatcher)

  def downs(hash: Seq[Byte]): Seq[String] = {
    withStatement(connection)(st => {
      ResultSetIterator(st.executeQuery(queryDownString(hash))).map(rs => {
        val index = rs.getInt(INDEX)
        val clob = rs.getClob(SCRIPT)
        val down = clob.getSubString(1, clob.length().toInt)
        (down, index)
      }).toSeq.sortBy(_._2).map(_._1)
    }, noDataCatcher)
  }

  def transaction: SqlTransaction = {
    connection.setAutoCommit(false)
    new SqlTransaction
  }
}
