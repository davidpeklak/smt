package smt.db

import scalaz._
import smt.util.ActionTypes
import smt.migration.{Script, MigrationInfo, Direction, Test}

object DbAction extends ActionTypes[Database] {

  type EA[+A] = Either[String, A]

  def eit[A](ea: EA[A]): SE[A] = ea match {
    case Left(s) => -\/(s)
    case Right(a) => \/-(a)
  }

  private def edbk[A](f: Database => EA[A]): EDKleisli[A] = EDKleisli(DKleisli(f andThen eit))

  private def eudbk[A](f: Database => (Option[String], Database)): EDKleisli[Unit] = EDKleisli(DKleisli(db => eit(f(db)._1.toLeft(()))))

  def state: EDKleisli[Seq[MigrationInfo]] = edbk(_.state)

  def downs(hash: Seq[Byte]): EDKleisli[Seq[Script]] = edbk(_.downs(hash))

  def add(migrationInfo: MigrationInfo): EDKleisli[Unit] = eudbk(_.add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EDKleisli[Unit] = eudbk(_.addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): EDKleisli[Unit] = eudbk(_.remove(hash))

  def removeDowns(migHash: Seq[Byte]): EDKleisli[Unit] = eudbk(_.removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): EDKleisli[Unit] = eudbk(_.applyScript(script, direction))

  def doTest(test: Test): EDKleisli[Unit] = edbk(db => test.run(db))
}
