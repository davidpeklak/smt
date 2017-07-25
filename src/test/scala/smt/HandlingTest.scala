package smt

import org.scalatest.FunSuite
import smt.MigrationGen._
import smt.DatabaseGen._
import org.scalacheck.{Gen, Prop}
import smt.db.{Connection, DatabaseId}
import smt.report.Reporter
import smt.util.Logger
import GenUtil.{params, seed}
import smt.migration.{Direction, Script}
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers._

import scala.collection.immutable
import scalaz.{\/, \/-}

class HandlingTest extends FunSuite {

  class ReporterMock extends Reporter {
    var called: Boolean = false

    def report(s: NamedMoveStates): String \/ Unit = \/- {
      called = true
    }
  }

  class LoggerMock extends Logger {
    override def info(s: => String): Unit = {}

    override def warn(s: => String): Unit = {}

    override def error(s: => String): Unit = {}
  }

  test("apply one migration - verify reporter is called") {

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(new ConnectionMock))

    val mig = migGen(databases).apply(params, seed()).get // bochn

    val reporter = new ReporterMock

    val logger = new LoggerMock

    val metaDb = new MetaDatabaseMock(new MetaConnectionMock)

    Handling.applyMigrationsAndReport(ms = Seq(mig), imo = None, arb = false, runTests = true, "user", "remark")(metaDb, databases, logger, List(reporter))

    assert(reporter.called)
  }

  trait CloseChecking {

    var closed: Boolean = true
    var closedTwice = false
    var openedTwice = false

    def open(): Unit = {
      if (!closed) openedTwice = true
      closed = false
    }

    def doClose(): Unit = {
      if (closed) closedTwice = true
      closed = true
    }
  }

  class CheckCloseConnectionMock extends ConnectionMock with CloseChecking {


    override def applyScript(logger: Logger)(script: Script, direction: Direction): \/[String, Unit] = \/-(())

    override def testScript(logger: Logger)(script: Script): \/[String, Unit] = \/-(())

    override def close(logger: Logger)(): \/[String, Unit] = {
      doClose()
      super.close(logger)()
    }
  }

  class CheckCloseDatabaseMock(val conn: CheckCloseConnectionMock) extends DatabaseMock(conn) {
    override def connection(): \/[String, Connection] = {
      conn.open()
      super.connection()
    }
  }

  class CheckCloseMetaConnectionMock extends MetaConnectionMock with CloseChecking {

    override def close(logger: Logger)(): \/[String, Unit] = {
      doClose()
      super.close(logger)()
    }
  }

  test("apply one migration - verify connections are closed") {

    val connection = new CheckCloseConnectionMock

    val databases = Map(DatabaseId("KAKTUS") -> new CheckCloseDatabaseMock(connection))

    val mig = migGen(databases).apply(params, seed()).get // bochn

    val logger = new LoggerMock

    val metaConnection = new CheckCloseMetaConnectionMock

    val metaDb = new MetaDatabaseMock(metaConnection)

    Handling.applyMigrationsAndReport(ms = Seq(mig), imo = None, arb = false, runTests = true, "user", "remark")(metaDb, databases, logger, List())

    assert(connection.closed === true)
    assert(connection.closedTwice === false)
    assert(connection.openedTwice === false)

    assert(metaConnection.closed === true)
  }

  test("apply many migrations to 10 connections - verify connections are closed") {

    def createDatabase(): CheckCloseDatabaseMock = {
      val conn = new CheckCloseConnectionMock
      new CheckCloseDatabaseMock(conn)
    }

    val gen = for {
      dbIds <- dbIdGen(10)
      databases: Map[DatabaseId, CheckCloseDatabaseMock] = dbIds.map(dbId => dbId -> createDatabase()).toMap
      migs <- listOfDistinctMig(databases)(100)
    } yield (databases, migs)

    check(forAll(gen) { case (databases, migs) => {
      val logger = new LoggerMock

      val metaConnection = new CheckCloseMetaConnectionMock

      val metaDb = new MetaDatabaseMock(metaConnection)

      Handling.applyMigrationsAndReport(ms = migs, imo = None, arb = false, runTests = true, "user", "remark")(metaDb, databases, logger, List())

      val connectionAssertions = for ((name, db) <- databases.toList) yield {
        Prop(db.conn.closed == true) :| s"connecion of $name closed" &&
          Prop(db.conn.closedTwice == false) :| s"connecion of $name not closed twice" &&
          Prop(db.conn.openedTwice === false) :| s"connecion of $name not opened twice"
      }

      Prop(metaConnection.closed == true) :| "metaconnection closed" && connectionAssertions.fold(Prop.proved)(_ && _)

    }
    })


  }
}
