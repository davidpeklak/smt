package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.Gen
import java.util.Date
import smt.db.{HasDbOnly, HasConnectionOnly, DbAction, Database}
import smt.migration._
import smt.migration.Group
import scalaz.{\/-, \/, -\/}
import smt.DBHandling.MigrationInfoWithDowns
import smt.migration.Test
import smt.migration.Migration

class DbHandlingTest extends FunSuite with PropTesting {

  test("apply one migration - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrations(ms = Seq(mig), arb = false, runTests = true)

    action.run.run(HasConnectionOnly(new ConnectionMock)).run
  }

  test("apply 10000 migrations fea - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrations(ms = Seq.fill(10000)(mig), arb = false, runTests = true)

    val conn = new ConnectionMock

    action.run.run(HasConnectionOnly(conn)).run

    assert(conn.addCount === 10000)
  }

  class ScriptRecordingConnectionMock extends ConnectionMock {
    var upScriptSeq: Seq[Script] = Seq()
    var downScriptSeq: Seq[Script] = Seq()
    var testScriptSeq: Seq[Script] = Seq()
    var downss: Seq[(Seq[Byte], Seq[Script])] = Seq()

    override def applyScript(script: Script, direction: Direction): String \/ Unit = {
      if (direction == Up) upScriptSeq = upScriptSeq :+ script
      else downScriptSeq = downScriptSeq :+ script
      if (script.content.contains("bad")) -\/("BAD")
      else \/-(())
    }


    override def testScript(script: Script): String \/ Unit = {
      testScriptSeq = testScriptSeq :+ script
      if (script.content.contains("bad")) -\/("BAD")
      else \/-(())
    }

    override def addDowns(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = {
      downss = downss :+ (migHash, downs)
      \/-(())
    }

  }

  test("apply one migration with test") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val mig = migGen.map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrations(ms = Seq(mig), arb = false, runTests = true)

    val conn= new ScriptRecordingConnectionMock

    action.run.run(HasConnectionOnly(conn)).run

    assert(conn.testScriptSeq.size === 1)
    assert(conn.testScriptSeq(0) === testScript)
  }

  test("apply one migration with test - but don't run tests") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val mig = migGen.map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrations(ms = Seq(mig), arb = false, runTests = false)

    val conn = new ScriptRecordingConnectionMock

    action.run.run(HasConnectionOnly(conn)).run

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

    val action = DBHandling.applyMigration(mig, MigrationHandling.hashMigration(mig, None))

    val r: (UpMoveState, DBHandling.SE[Unit]) = action.run.run(HasConnectionOnly(conn)).run

    r._2 match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(conn.upScriptSeq === Seq(good(1), good(2), good(5), bad))
    assert(conn.downss.size === 1)
    assert(conn.downss(0)._2 === Seq(good(3), good(4)))
  }
  
  test("revert one migration that fails") {
    val downs = Seq(good(1), good(2), bad, good(3), good(4))
    val migInfo = MigrationInfo("migName", Seq[Byte](), new Date)

    val conn = new ScriptRecordingConnectionMock

    val action = DBHandling.revertMigration(MigrationInfoWithDowns(migInfo, downs))

    val r: (DownMoveState, DBHandling.SE[Unit]) = action.run.run(HasConnectionOnly(conn)).run

    r._2 match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(conn.downScriptSeq === Seq(good(4), good(3), bad))
    assert(conn.downss.size === 1)
    assert(conn.downss(0)._2 === Seq(good(1), good(2)))
  }
}
