package smt

import org.scalatest.FunSuite
import MigrationGen._
import org.scalacheck.Gen
import scalaz.Free

class DbHandlingTest extends FunSuite with PropTesting {

  class DatabaseMock extends Database {
    def state: Either[Failure, Seq[MigrationInfo]] = Right(Seq())

    def downs(hash: Seq[Byte]): Either[Failure, Seq[Script]] = Right(Seq())

    def add(migrationInfo: MigrationInfo): (Option[Failure], Database) = (None, this)

    def addDowns(migHash: Seq[Byte], downs: Seq[Script]): (Option[Failure], Database) = (None, this)

    def remove(hash: Seq[Byte]): (Option[Failure], Database) = (None, this)

    def removeDowns(migHash: Seq[Byte]): (Option[Failure], Database) = (None, this)

    def applyScript(script: Script, direction: Direction): (Option[Failure], Database) = (None, this)
  }

  test("apply one migration - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq(mig), arb = false)

    FreeDbAction.run(action, new DatabaseMock)
  }

  /*test("apply 50 migrations - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq.fill(50)(mig), arb = false)

    FreeDbAction.run(action, new DatabaseMock)
  } */

  /*test("apply 500 migrations ea - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq.fill(500)(mig), arb = false)

    import FreeDbAction._

    val sAction: Free[EA, Unit] = action.mapSuspension(ffEaTransformation(new DatabaseMock))

    sAction.go(extractEither[Unit])
  } */

  test("apply 10000 migrations fea - smoke") {

    val mig = migGen.apply(Gen.Params()).get // bochn

    val action = DBHandling.applyMigrationsImplAction(ms = Seq.fill(10000)(mig), arb = false)

    import FreeDbAction._

    val sAction: Free[FEA, Unit] = action.mapSuspension(ffFeaTransformation(new DatabaseMock))

    sAction.go(extractFEither[Unit])
  }
}
