package smt

import org.scalatest.FunSuite
import smt.MigrationGen._
import org.scalacheck.Gen
import smt.report.Reporter
import scalaz.{\/-, \/}

class HandlingTest extends FunSuite {

  class ReporterMock extends Reporter {
    var called: Boolean = false

    def report(s: NamedMoveStates): String \/ Unit = \/-{
      called = true
    }
  }

  test("apply one migration - verify reporter is called") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val reporter = new ReporterMock

    val action = Handling.applyMigrationsAndReport(ms = Seq(mig), arb = false, runTests = true)

    action.run(Handling.Dep(new DatabaseMock, List(reporter))).run

    assert(reporter.called)
  }
}
