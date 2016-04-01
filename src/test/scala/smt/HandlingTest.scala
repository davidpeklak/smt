package smt

import org.scalatest.FunSuite
import smt.MigrationGen._
import org.scalacheck.Gen
import smt.db.DatabaseId
import smt.report.Reporter
import scalaz.{\/-, \/}
import sbt.{Level, Logger}

class HandlingTest extends FunSuite {

  class ReporterMock extends Reporter {
    var called: Boolean = false

    def report(s: NamedMoveStates): String \/ Unit = \/- {
      called = true
    }
  }

  class LoggerMock extends Logger {
    def trace(t: => Throwable): Unit = ()

    def log(level: Level.Value, message: => String): Unit = ()

    def success(message: => String): Unit = ()
  }

  test("apply one migration - verify reporter is called") {

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(new ConnectionMock))

    val mig = migGen(databases).apply(Gen.Params()).get // bochn

    val reporter = new ReporterMock

    val logger = new LoggerMock

    val metaDb = new MetaDatabaseMock(new MetaConnectionMock)

    Handling.applyMigrationsAndReport(ms = Seq(mig), imo = None, arb = false, runTests = true, "user", "remark")(metaDb, databases, logger, List(reporter))

    assert(reporter.called)
  }
}
