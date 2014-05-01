package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.Gen
import java.util.Date
import smt.db.{DbAction, Database}
import smt.migration._
import smt.migration.Group
import scalaz.-\/
import smt.DBHandling.MigrationInfoWithDowns
import smt.migration.Test
import smt.migration.Migration

class DbHandlingTest extends FunSuite with PropTesting {

  class DatabaseMock extends Database {

    var addCount: Int = 0

    def state: Either[Failure, Seq[MigrationInfo]] = Right(Seq())

    def downs(hash: Seq[Byte]): Either[Failure, Seq[Script]] = Right(Seq())

    def add(migrationInfo: MigrationInfo): (Option[Failure], Database) = {
      addCount = addCount + 1
      (None, this)
    }

    def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database) = (None, this)

    def remove(hash: Seq[Byte]): (Option[Failure], Database) = (None, this)

    def removeDowns(migHash: Seq[Byte]): (Option[Failure], Database) = (None, this)

    def applyScript(script: Script, direction: Direction): (Option[Failure], Database) = (None, this)

    def testScript(script: Script): (Option[Failure], Database) = (None, this)
  }

  test("apply one migration - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq(mig), arb = false, runTests = true)

    action.run.run(new DatabaseMock).run
  }

  test("apply 10000 migrations fea - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq.fill(10000)(mig), arb = false, runTests = true)

    import DbAction._

    val db = new DatabaseMock

    action.run.run(db).run

    assert(db.addCount === 10000)
  }

  class ScriptRecordingDbMock extends DatabaseMock {
    var upScriptSeq: Seq[Script] = Seq()
    var downScriptSeq: Seq[Script] = Seq()
    var testScriptSeq: Seq[Script] = Seq()
    var downss: Seq[(Seq[Byte], Seq[Script])] = Seq()

    override def applyScript(script: Script, direction: Direction): (Option[Failure], Database) = {
      if (direction == Up) upScriptSeq = upScriptSeq :+ script
      else downScriptSeq = downScriptSeq :+ script
      if (script.content.contains("bad")) (Some("BAD"), this)
      else (None, this)
    }


    override def testScript(script: Script): (Option[ScriptRecordingDbMock#Failure], Database) = {
      testScriptSeq = testScriptSeq :+ script
      if (script.content.contains("bad")) (Some("BAD"), this)
      else (None, this)
    }

    override def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database) = {
      downss = downss :+ (migHash, downs)
      (None, this)
    }

  }

  test("apply one migration with test") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val mig = migGen.map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq(mig), arb = false, runTests = true)

    val db = new ScriptRecordingDbMock

    action.run.run(db).run

    assert(db.testScriptSeq.size === 1)
    assert(db.testScriptSeq(0) === testScript)
  }

  test("apply one migration with test - but don't run tests") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val mig = migGen.map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq(mig), arb = false, runTests = false)

    val db = new ScriptRecordingDbMock

    action.run.run(db).run

    assert(db.testScriptSeq.size === 0)
  }

  def good(i: Int) = Script("good" + i.toString, "good")
  val bad = Script("bad", "bad")

  test("apply one migration that fails") {
    val mig = Migration("mig1", Seq(
      Group(Seq(good(1), good(2)), Seq(good(3), good(4))),
      Group(Seq(good(5), bad), Seq(good(6), good(7))),
      Group(Seq(good(8), good(9)), Seq(good(10), good(11)))
    ), Seq())

    val db = new ScriptRecordingDbMock

    val action = DBHandling.applyMigration(mig, MigrationHandling.hashMigration(mig, None))

    val r: (UpMoveState, DbAction.SE[Unit]) = action.run.run(db).run

    r._2 match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(db.upScriptSeq === Seq(good(1), good(2), good(5), bad))
    assert(db.downss.size === 1)
    assert(db.downss(0)._2 === Seq(good(3), good(4)))
  }
  
  test("revert one migration that fails") {
    val downs = Seq(good(1), good(2), bad, good(3), good(4))
    val migInfo = MigrationInfo("migName", Seq[Byte](), new Date)

    val db = new ScriptRecordingDbMock

    val action = DBHandling.revertMigration(MigrationInfoWithDowns(migInfo, downs))

    val r: (DownMoveState, DbAction.SE[Unit]) = action.run.run(db).run

    r._2 match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(db.downScriptSeq === Seq(good(4), good(3), bad))
    assert(db.downss.size === 1)
    assert(db.downss(0)._2 === Seq(good(1), good(2)))
  }
}
