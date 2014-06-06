package smt

import org.scalatest.FunSuite
import smt.MigrationGen._
import org.scalacheck.Gen
import smt.report.Reporter
import scalaz.{\/-, \/}
import sbt.{Level, Logger}
import smt.report.ReportersAction.HasReporters
import smt.describe.DescribeAction.HasLogger
import smt.db.DbAction.HasDb
import smt.db.AddAction.{HasUser, HasRemark}

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

  lazy val handling = new Handling[HandlingDep] {
    lazy val hasDb: HasDb[HandlingDep] = _.db
    lazy val hasLogger: HasLogger[HandlingDep] = _.logger
    lazy val hasReporters: HasReporters[HandlingDep] = _.rps
    lazy val hasUser: HasUser[HandlingDep] = _.user
    lazy val hasRemark: HasRemark[HandlingDep] = _.remark
  }

  test("apply one migration - verify reporter is called") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val reporter = new ReporterMock

    val logger = new LoggerMock

    val action = handling.applyMigrationsAndReport(ms = Seq(mig), arb = false, runTests = true)

    action.run(HandlingDep(new DatabaseMock(new ConnectionMock), List(reporter), logger, "fooUser", None)).run

    assert(reporter.called)
  }
}
