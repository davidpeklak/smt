package smt

import scalaz._
import scalaz.std.list._
import scalaz.std.function._
import UpMoveState._
import scalaz.concurrent.Future

object DbAction {

  type SE[A] = (String \/ A)

  type DbKleisli[+A] = Kleisli[Future, Database, A]

  def DbKleisli[A](f: Database => A): DbKleisli[A] = Kleisli[Future, Database, A](db => Future.delay(f(db)))

  type EDbKleisli[+A] = EitherT[DbKleisli, String, A]

  def EDbKleisli[A](a: DbKleisli[SE[A]]) = EitherT[DbKleisli, String, A](a)

  def eit[A](ea: EA[A]): SE[A] = ea match {
    case Left(s) => -\/(s)
    case Right(a) => \/-(a)
  }

  def point[A](a: A): EDbKleisli[A] = EDbKleisli(Kleisli[Future, Database, SE[A]](db => Future.delay(\/-(a))))

  private def edbk[A](f: Database => EA[A]): EDbKleisli[A] = EDbKleisli(DbKleisli(f andThen eit))

  private def eudbk[A](f: Database => (Option[String], Database)): EDbKleisli[Unit] = EDbKleisli(DbKleisli(db => eit(f(db)._1.toLeft(()))))

  def state: EDbKleisli[Seq[MigrationInfo]] = edbk(_.state)

  def downs(hash: Seq[Byte]): EDbKleisli[Seq[Script]] = edbk(_.downs(hash))

  def add(migrationInfo: MigrationInfo): EDbKleisli[Unit] = eudbk(_.add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EDbKleisli[Unit] = eudbk(_.addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): EDbKleisli[Unit] = eudbk(_.remove(hash))

  def removeDowns(migHash: Seq[Byte]): EDbKleisli[Unit] = eudbk(_.removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): EDbKleisli[Unit] = eudbk(_.applyScript(script, direction))

  def doTest(test: Test): EDbKleisli[Unit] = edbk(db => test.run(db))

  def failure(f: String): EDbKleisli[Nothing] = EDbKleisli(DbKleisli(_ => -\/(f)))

  trait WriterTypes[W] {

    type WDbKleisli[+A] = WriterT[DbKleisli, W, A]
    
    def WDbKleisli[A](a: DbKleisli[(W, A)]): WDbKleisli[A] = WriterT[DbKleisli, W, A](a)

    type EWDbKleisli[+A] = EitherT[WDbKleisli, String, A]

    def EWDbKleisli[A](wa: WDbKleisli[SE[A]]) = EitherT[WDbKleisli, String, A](wa)

    def lift[A](dbKleisli: DbKleisli[A])(implicit W: Monoid[W]): WDbKleisli[A] = WriterT.writerTMonadTrans[W].liftM(dbKleisli)

    def liftE[A](edbKleisli: EDbKleisli[A])(implicit W: Monoid[W]): EWDbKleisli[A] = EWDbKleisli(lift(edbKleisli.run))
    
    def putE[A](edbKleisli: EDbKleisli[A])(w: W): EWDbKleisli[A] = EitherT[WDbKleisli, String, A](WriterT.put(edbKleisli.run)(w))


    val EWSyntax = EitherTWriterT.eitherTWriterTSyntax[DbKleisli, String, W]
  }

  def writerTypes[S]: WriterTypes[S] = new WriterTypes[S] {}

  type EA[+A] = Either[String, A]
}
