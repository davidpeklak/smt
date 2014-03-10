package smt

import scalaz.StateT
import scalaz.std.either._

object DbAction {
  type EA[+A] = Either[String, A]

  type DbAction[A] = StateT[EA, Database, A]

  private def apply[A](f: Database => Either[String, (Database, A)]): StateT[EA, Database, A] = StateT[EA, Database, A](f)

  implicit val EAM = eitherMonad[String]
  implicit val SEAM = StateT.stateTMonadState[Database, EA]

  private def getFromDb[A](f: Database => EA[A]): DbAction[A] = DbAction(db => f(db) match {
    case Left(s) => Left(s)
    case Right(a) => Right(db, a)
  })

  private def modify(f: Database => (Option[String], Database)): DbAction[Unit] = DbAction(db => {
    f(db) match {
      case (Some(err), db2) => Left(err)
      case (None, db2) => Right((db2, ()))
    }
  })

  def state: DbAction[Seq[MigrationInfo]] = getFromDb(_.state)

  def downs(hash: Seq[Byte]): DbAction[Seq[Script]] = getFromDb(_.downs(hash))

  def add(migrationInfo: MigrationInfo): DbAction[Unit] = modify(_.add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): DbAction[Unit] = modify(_.addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): DbAction[Unit] = modify(_.remove(hash))

  def removeDowns(migHash: Seq[Byte]): DbAction[Unit] = modify(_.removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): DbAction[Unit] = modify(_.applyScript(script, direction: Direction))

  def failure(f: => String): DbAction[Nothing] = StateT[EA, Database, Nothing](db => Left(f))
}
