package smt

import org.scalatest.FunSuite
import smt.MigrationGen._
import org.scalacheck.Gen
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

    val mig = migGen.apply(Gen.Params()).get // bochn

    val reporter = new ReporterMock

    val logger = new LoggerMock

    Handling.applyMigrationsAndReport(ms = Seq(mig), imo = None, arb = false, runTests = true, "user", "remark")(new DatabaseMock(new ConnectionMock), logger, List(reporter), new NamedMoveStatesHolder())

    assert(reporter.called)
  }
}
