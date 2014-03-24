package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.Gen
import scalaz.{-\/, Free}

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

    FreeDbAction.run(action)(new DatabaseMock)
  }


  test("apply 10000 migrations fea - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq.fill(10000)(mig), arb = false, runTests = true)

    import FreeDbAction._

    val db = new DatabaseMock

    FreeDbAction.run(action)(db)

    assert(db.addCount === 10000)
  }

  test("apply one migration that fails") {
    class ScriptRecordingDbMock extends DatabaseMock {
      var scriptSeq: Seq[Script] = Seq()
      var downss: Seq[(Seq[Byte], Seq[Script])] = Seq()

      override def applyScript(script: Script, direction: Direction): (Option[Failure], Database) = {
        scriptSeq = scriptSeq :+ script
        if (script.content.contains("bad")) (Some("BAD"), this)
        else (None, this)
      }

      override def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database) = {
        downss = downss :+ (migHash, downs)
        (None, this)
      }

    }

    def good(i: Int) = Script("good" + i.toString, "good")
    val bad = Script("bad", "bad")

    val mig = Migration("mig1", Seq(
      Group(Seq(good(1), good(2)), Seq(good(3), good(4))),
      Group(Seq(good(5), bad), Seq(good(6), good(7))),
      Group(Seq(good(8), good(9)), Seq(good(10), good(11)))
    ), Seq())

    val db = new ScriptRecordingDbMock

    val action = DBHandling.applyMigration(mig, MigrationHandling.hashMigration(mig, None))

    val r: DbAction.SE[Unit] = FreeDbAction.run(action)(db)

    r match {
      case -\/(f) => println(f)
      case _ => ()
    }

    assert(db.scriptSeq === Seq(good(1), good(2), good(5), bad))
    assert(db.downss.size === 1)
    assert(db.downss(0)._2 === Seq(good(3), good(4)))
  }
}
