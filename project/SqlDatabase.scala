import java.sql.{Connection => JConnection, Statement, ResultSet, DriverManager, SQLException}
import java.util.Date
import util.control.Exception.{Catcher, catching}
import Util._
import collection.Map.empty

class SqlDatabase(connectIdentifier: String, driverClass: Class[_]) extends Database {

  def ??? = throw new Exception("Not implemented")

  def withStatement[U](c: JConnection)(f: Statement => U, ca: Catcher[U] = empty[Throwable, U]) = {
    val st = c.createStatement()
    catching(ca).andFinally(st.close())(f(st))

    /*try {
      f(st)
    }
    catch {
      case e: SQLException => {
        println("SqlException with Code: " + e.getErrorCode)
        throw e
      }
      case e: Exception => {
        println("Aundare Exception: " + e.getClass.getSimpleName)
        throw e
      }
    }
    finally {
      st.close()
    }  */
  }

  val NAME = "NAME"
  val HASH = "HASH"
  val TIME = "TIME"
  val SEQ = "SEQ"

  val createMigrationTableStringOracle =
    """
    BEGIN
      EXECUTE IMMEDIATE '
        CREATE TABLE MIGRATION (
          SEQ NUMBER(10),
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
        CREATE TABLE MIGRATION (
          SEQ NUMBER(10),
          NAME VARCHAR(128),
          HASH VARCHAR(40),
          TIME NUMBER(15)
        )
    """

  val queryMigrationTableString =
    """
    SELECT * FROM MIGRATION
    """

  val tableExistsCatcher: Catcher[Unit] = {
    case e: SQLException if e.getErrorCode == 42101 && e.getMessage.contains(("Table \"MIGRATION\" already exists")) => {
      ()
    }
  }

  lazy val connection = {
    val connection = DriverManager.getConnection(connectIdentifier)
    withStatement(connection)(_.execute(createMigrationTableString), tableExistsCatcher)
    connection
  }

  class SqlTransaction extends Transaction {
    type T = SqlTransaction
    type DB = SqlDatabase

    def add(migrationInfo: MigrationInfo): Either[String, SqlTransaction] = ???

    def addDowns(migHash: Seq[Byte], downs: Seq[String]): Either[String, SqlTransaction] = ???

    def remove(hash: Seq[Byte]): Either[String, SqlTransaction] = ???

    def removeDowns(migHash: Seq[Byte]): Either[String, SqlTransaction] = ???

    def apply(script: String): Either[String, SqlTransaction] = ???

    def commit: SqlDatabase = ???
  }

  type T = SqlTransaction

  def state: Seq[MigrationInfo] = withStatement(connection)(st => {
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

  def downs(hash: Seq[Byte]): Seq[String] = ???

  def transaction: SqlTransaction = ???
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