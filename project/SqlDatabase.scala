import java.sql.{Connection => JConnection, Statement, ResultSet, DriverManager, SQLException}
import java.util.Date
import util.control.Exception.{Catcher, catching, allCatch}
import Util._
import collection.Map.empty

class SqlDatabase(connectIdentifier: String, driverClass: Class[_]) extends Database {
  sqlDatabase =>

  def ??? = throw new Exception("Not implemented")

  def withStatement[U](c: JConnection)(f: Statement => U, ca: Catcher[U] = empty[Throwable, U]) = {
    val st = c.createStatement()
    catching(ca).andFinally(st.close())(f(st))
  }

  val NAME = "NAME"
  val HASH = "HASH"
  val TIME = "TIME"
  val SEQ = "SEQU"

  val createMigrationTableStringOracle =
    """
    BEGIN
      EXECUTE IMMEDIATE '
        CREATE TABLE MIRGATION (
          SEQU NUMBER(10),
          NAME VARCHAR(128),
          HASH VARCHAR(40),
          TIME NUMBER(15)
        )';
      EXCEPTION
        WHEN OTHERS THEN
          IF SQLCODE = -955 THEN NULL;
          ELSE RAISE;
          END IF;
      END;
    """

  val createMigrationTableString =
    """
        CREATE TABLE MIRGATION (
          SEQU NUMBER(10),
          NAME VARCHAR(128),
          HASH VARCHAR(40),
          TIME NUMBER(15)
        )
    """

  val createDownsTableString =
    """
        CREATE TABLE DOWN (
          HASH VARCHAR(40),
          SEQU NUMBER(10),
          SCRIP CLOB
        )
    """

  val queryMigrationTableString =
    """
    SELECT * FROM MIRGATION
    """

  def insertMigrationString(mi: MigrationInfo, seq: Long): String = {
    "INSERT INTO MIRGATION VALUES ( " + seq + ", '" + mi.name + "', '" + bytesToHex(mi.hash) + "', " + mi.dateTime.getTime + " )"
  }

  def removeMigrationString(hash: Seq[Byte]): String = {
    "DELETE FROM MIRGATION WHERE HASH = '" + bytesToHex(hash) + "'"
  }

  val tableExistsCatcher: Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 42101 && e.getMessage.contains(("Table \"MIRGATION\" already exists")) => {
      ()
    }
  }

  lazy val connection = {
    val connection = DriverManager.getConnection(connectIdentifier)
    withStatement(connection)(_.execute(createMigrationTableString), tableExistsCatcher)
    withStatement(connection)(_.execute(createDownsTableString), tableExistsCatcher)
    connection
  }

  class SqlTransaction extends Transaction {
    type T = SqlTransaction
    type DB = SqlDatabase

    private def exceptionToEither(block: => Unit): Either[String, SqlTransaction] = allCatch.either(block).left.map(_.getMessage).right.map(_ => this)

    def add(migrationInfo: MigrationInfo): Either[String, SqlTransaction] = exceptionToEither {
      val mm = withStatement(connection)(st => {
        ResultSetIterator(st.executeQuery(queryMigrationTableString)).map(rs => {
          rs.getLong(SEQ)
        }).toSeq.sorted.headOption
      }, {
        case e: SQLException => None
      })

      val mi = mm.getOrElse(0L) + 1L
      println("adding migration " + mi + ", " + migrationInfo)

      withStatement(connection)(_.execute(insertMigrationString(migrationInfo, mi)))
    }

    def addDowns(migHash: Seq[Byte], downs: Seq[String]): Either[String, SqlTransaction] = exceptionToEither((println("addDowns")))

    def remove(hash: Seq[Byte]): Either[String, SqlTransaction] = exceptionToEither({
      println("removing " + bytesToHex(hash))
      withStatement(connection)(_.execute(removeMigrationString(hash)))
    })

    def removeDowns(migHash: Seq[Byte]): Either[String, SqlTransaction] = exceptionToEither((println("removeDowns")))

    def apply(script: String): Either[String, SqlTransaction] = exceptionToEither({
      println("applying script")
      withStatement(connection)(_.execute(script))
    })

    def commit: SqlDatabase = sqlDatabase
  }

  type T = SqlTransaction

  def state: Seq[MigrationInfo] = withStatement(connection)(st => {
    println("state")
    ResultSetIterator(st.executeQuery(queryMigrationTableString)).map(rs => {
      (MigrationInfo(
        name = rs.getString(NAME),
        hash = hexToBytes(rs.getString(HASH)),
        dateTime = new Date(rs.getLong(TIME))
      ),
        rs.getLong(SEQ)
        )
    }).toSeq.sortBy(_._2).map(_._1)
  }, {
    case e: SQLException if e.getErrorCode == 2000 => Seq()
  })

  def downs(hash: Seq[Byte]): Seq[String] = Seq()

  def transaction: SqlTransaction = new SqlTransaction
}

object ResultSetIterator {
  def apply(rs: ResultSet): Iterator[ResultSet] = new Iterator[ResultSet] {
    def hasNext: Boolean = !rs.isLast

    def next(): ResultSet = {
      rs.next()
      rs
    }
  }
}