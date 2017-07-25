package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.{Gen, Prop}
import java.util.Date

import smt.MetaConnectionHandling.MigrationInfoWithDowns
import smt.migration._
import smt.migration.Group

import scalaz.{-\/, \/, \/-}
import smt.migration.Test
import smt.migration.Migration
import smt.db.{Connection, DatabaseId}
import smt.util.Logger
import smt.GenUtil._
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty.Params
import org.scalatest.prop.Checkers

class DbHandlingTest extends FunSuite with Checkers with PropTesting {

  lazy val logger: Logger = new Logger {
    def info(s: =>String): Unit = {}

    def warn(s: =>String): Unit = {}

    override def error(s: => String): Unit = {}
  }

  test("apply one migration - smoke") {

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(new ConnectionMock))

    check(forAll(migGen(databases))(mig => {
      MulipleDatabasesHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(new MetaConnectionMock, databases, logger, new NamedMoveStatesHolder())
    }))
  }

  test("apply 10000 migrations - smoke") {

    val databases = Map(DatabaseId("KAKTUS") -> new DatabaseMock(new ConnectionMock))

    check(forAll(migGen(databases))(mig => {

      val metaConn = new MetaConnectionMock

      MulipleDatabasesHandling.applyMigrations(ms = Seq.fill(10000)(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(metaConn, databases, logger, new NamedMoveStatesHolder())

      metaConn.addCount == 10000
    }))
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

    val gen = for {
      dbId <- nonEmptyAlphaStr
      conn = new ScriptRecordingConnectionMock
      databases = Map(DatabaseId(dbId) -> new DatabaseMock(conn))
      mig <- migGen(databases).map(_.copy(tests = Seq(test)))
    } yield (conn, databases, mig)

    check(forAll(gen) {case (conn, databases, mig) => {
      MulipleDatabasesHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = true, user = "user", remark = "remark")(new MetaConnectionMock, databases, logger, new NamedMoveStatesHolder())

      Prop(conn.testScriptSeq.size == 1) :| s"size of testScriptSeq not 1 for migration: $mig" &&
        Prop(conn.testScriptSeq(0) == testScript) :| s"testScriptSeq(0) does not equal testScript for migration: $mig"
    }
    })
  }

  test("apply one migration with test - but don't run tests") {
    val testScript = Script("test", "testScript")

    val test: Test = ScriptTest.scriptTest(testScript)

    val gen = for {
      dbId <- nonEmptyAlphaStr
      conn = new ScriptRecordingConnectionMock
      databases = Map(DatabaseId(dbId) -> new DatabaseMock(conn))
      mig <- migGen(databases).map(_.copy(tests = Seq(test)))
    } yield (conn, databases, mig)

    check(forAll(gen) {case (conn, databases, mig) => {
      MulipleDatabasesHandling.applyMigrations(ms = Seq(mig), imo = None, arb = false, runTests = false, user = "user", remark = "remark")(new MetaConnectionMock, databases, logger, new NamedMoveStatesHolder())

      Prop(conn.testScriptSeq.size == 0) :| s"size of testScriptSeq not 0 for migration: $mig"
    }
    })
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

