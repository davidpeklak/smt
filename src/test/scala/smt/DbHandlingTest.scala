package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.Gen
import java.util.Date
import smt.migration._
import smt.migration.Group
import scalaz.{\/-, \/, -\/}
import smt.migration.Test
import smt.migration.Migration
import smt.db.Connection
import sbt.{Logger, Level}

class DbHandlingTest extends FunSuite with PropTesting {

  lazy val logger: Logger = new Logger {
    def log(level: Level.Value, message: => String): Unit = {}

    def success(message: => String): Unit = {}

    def trace(t: => Throwable): Unit = {}
  }

  test("apply one migration - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    AddHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(new ConnectionMock, logger, new NamedMoveStatesHolder())
  }

  test("apply 10000 migrations fea - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val conn = new ConnectionMock

    AddHandling.applyMigrations(ms = Seq.fill(10000)(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(conn, logger, new NamedMoveStatesHolder())

    assert(conn.addCount === 10000)
  }

  class ScriptRecordingConnectionMock extends ConnectionMock {
    var upScriptSeq: Seq[Script] = Seq()
    var downScriptSeq: Seq[Script] = Seq()
    var testScriptSeq: Seq[Script] = Seq()
    var downss: Seq[(Seq[Byte], Seq[Script])] = Seq()

    override def applyScript(logger: Logger)(script: Script, direction: Direction): String \/ Unit = {
      if (direction == Up) upScriptSeq = upScriptSeq :+ script
      else downScriptSeq = downScriptSeq :+ script
      if (script.content.contains("bad")) -\/("BAD")
      else \/-(())
    }


    override def testScript(logger: Logger)(script: Script): String \/ Unit = {
      testScriptSeq = testScriptSeq :+ script
      if (script.content.contains("bad")) -\/("BAD")
      else \/-(())
    }

    override def addDowns(logger: Logger)(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = {
      downss = downss :+(migHash, downs)
      \/-(())
    }
  }

  test("apply one migration with test") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val mig = migGen.map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    val conn = new ScriptRecordingConnectionMock

    AddHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(conn, logger, new NamedMoveStatesHolder())

    assert(conn.testScriptSeq.size === 1)
    assert(conn.testScriptSeq(0) === testScript)
  }

  test("apply one migration with test - but don't run tests") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val mig = migGen.map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    val conn = new ScriptRecordingConnectionMock

    AddHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = false, user = "user", remark = "remark")(conn, logger, new NamedMoveStatesHolder())

    assert(conn.testScriptSeq.size === 0)
  }

  def good(i: Int) = Script("good" + i.toString, "good")

  val bad = Script("bad", "bad")

  test("apply one migration that fails") {
    val mig = Migration("mig1", Seq(
      Group(Seq(good(1), good(2)), Seq(good(3), good(4))),
      Group(Seq(good(5), bad), Seq(good(6), good(7))),
      Group(Seq(good(8), good(9)), Seq(good(10), good(11)))
    ), Seq())

    val conn = new ScriptRecordingConnectionMock

    val r = AddHandling.applyMigration(mig, MigrationHandling.hashMigration(mig, None), "user", "remark")(conn, logger, new UpMoveStateHolder())

    r match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(conn.upScriptSeq === Seq(good(1), good(2), good(5), bad))
    assert(conn.downss.size === 1)
    assert(conn.downss(0)._2 === Seq(good(3), good(4)))
  }

  test("revert one migration that fails") {
    val downs = Seq(good(1), good(2), bad, good(3), good(4))
    val migInfo = MigrationInfo("migName", Seq[Byte](), new Date, None, None)

    val conn = new ScriptRecordingConnectionMock

    val r = AddHandling.revertMigration(MigrationInfoWithDowns(migInfo, downs), "user", "remark")(conn, logger, new DownMoveStateHolder())


    r match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(conn.downScriptSeq === Seq(good(4), good(3), bad))
    assert(conn.downss.size === 1)
    assert(conn.downss(0)._2 === Seq(good(1), good(2)))
  }
}
