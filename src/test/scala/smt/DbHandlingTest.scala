package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.Gen
import java.util.Date
import smt.MetaConnectionHandling.MigrationInfoWithDowns
import smt.migration._
import smt.migration.Group
import scalaz.{\/-, \/, -\/}
import smt.migration.Test
import smt.migration.Migration
import smt.db.{DatabaseId, Connection}
import sbt.{Logger, Level}

class DbHandlingTest extends FunSuite with PropTesting {

  lazy val logger: Logger = new Logger {
    def log(level: Level.Value, message: => String): Unit = {}

    def success(message: => String): Unit = {}

    def trace(t: => Throwable): Unit = {}
  }

  test("apply one migration - smoke") {

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(new ConnectionMock))

    val mig = migGen(databases).apply(Gen.Params()).get // bochn

    MulipleDatabasesHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(new MetaConnectionMock, databases, logger, new NamedMoveStatesHolder())
  }

  test("apply 10000 migrations - smoke") {

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(new ConnectionMock))

    val mig = migGen(databases).apply(Gen.Params()).get // bochn

    val metaConn = new MetaConnectionMock

    MulipleDatabasesHandling.applyMigrations(ms = Seq.fill(10000)(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(metaConn, databases, logger, new NamedMoveStatesHolder())

    assert(metaConn.addCount === 10000)
  }

  class ScriptRecordingConnectionMock extends ConnectionMock {
    var upScriptSeq: Seq[Script] = Seq()
    var downScriptSeq: Seq[Script] = Seq()
    var testScriptSeq: Seq[Script] = Seq()

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
  }

  test("apply one migration with test") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val conn = new ScriptRecordingConnectionMock

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(conn))

    val mig = migGen(databases).map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    MulipleDatabasesHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(new MetaConnectionMock, databases, logger, new NamedMoveStatesHolder())

    assert(conn.testScriptSeq.size === 1, s"Failed for migration: $mig")
    assert(conn.testScriptSeq(0) === testScript)
  }

  test("apply one migration with test - but don't run tests") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val conn = new ScriptRecordingConnectionMock

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(conn))

    val mig = migGen(databases).map(_.copy(tests = Seq(test))).apply(Gen.Params()).get // bochn

    MulipleDatabasesHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = false, user = "user", remark = "remark")(new MetaConnectionMock, databases, logger, new NamedMoveStatesHolder())

    assert(conn.testScriptSeq.size === 0)
  }

  def good(i: Int) = Script("good" + i.toString, "good")

  val bad = Script("bad", "bad")

  class DownsRecordingMetaConnectionMock extends MetaConnectionMock {
    var downss: Seq[(Seq[Byte], Seq[Script])] = Seq()

    override def addDowns(logger: Logger)(migHash: Seq[Byte], downs: Seq[Script]): String \/ Unit = {
      downss = downss :+(migHash, downs)
      \/-(())
    }
  }

  test("apply one migration that fails") {
    val dbId = DatabaseId("KAKTUS")

    val mig = Migration(dbId, "mig1", Seq(
      Group(Seq(good(1), good(2)), Seq(good(3), good(4))),
      Group(Seq(good(5), bad), Seq(good(6), good(7))),
      Group(Seq(good(8), good(9)), Seq(good(10), good(11)))
    ), Seq())

    val conn = new ScriptRecordingConnectionMock

    val metaConn = new DownsRecordingMetaConnectionMock

    SingleConnectionHandling.applyMigration(mig, MigrationHandling.hashMigration(mig, None), "user", "remark")(metaConn, conn, logger, new UpMoveStateHolder())

    assert(conn.upScriptSeq === Seq(good(1), good(2), good(5), bad))
    assert(metaConn.downss.size === 1)
    assert(metaConn.downss(0)._2 === Seq(good(3), good(4)))
  }

  test("revert one migration that fails") {
    val dbId = DatabaseId("KAKTUS")

    val downs = Seq(good(1), good(2), bad, good(3), good(4))
    val migInfo = MigrationInfo(dbId, "migName", Seq[Byte](), new Date, None, None)

    val conn = new ScriptRecordingConnectionMock

    val metaConn = new DownsRecordingMetaConnectionMock

    SingleConnectionHandling.revertMigration(MigrationInfoWithDowns(migInfo, downs), "user", "remark")(metaConn, conn, logger, new DownMoveStateHolder())

    assert(conn.downScriptSeq === Seq(good(4), good(3), bad))
    assert(metaConn.downss.size === 1)
    assert(metaConn.downss(0)._2 === Seq(good(1), good(2)))
  }
}

