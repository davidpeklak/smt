package smt

import scalaz._
import scalaz.std.list._
import scalaz.std.function._
import UpMoveState._

object FreeDbAction {

  import DbAction._

  import FreeFunctor._

  type DbActionFreeFunctor[+A] = FreeFunctor[DbAction, A]

  type FreeDbAction[+A] = Free[DbActionFreeFunctor, A]

  type EFreeDbAction[+A] = EitherT[FreeDbAction, String, A]
  def EFreeDbAction[A](a: FreeDbAction[SE[A]]) = EitherT[FreeDbAction, String, A](a)

  private def req[A](dba: DbAction[A]): FreeDbAction[A] = request[DbAction, A](dba)

  private def ereq[A](dba: DbAction[SE[A]]): EFreeDbAction[A] = EFreeDbAction(req(dba))

  def state: EFreeDbAction[Seq[MigrationInfo]] = ereq(State)

  def downs(hash: Seq[Byte]): EFreeDbAction[Seq[Script]] = ereq(Downs(hash))

  def add(migrationInfo: MigrationInfo): EFreeDbAction[Unit] = ereq(Add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EFreeDbAction[Unit] = ereq(AddDowns(migHash, downs))

  def remove(hash: Seq[Byte]): EFreeDbAction[Unit] = ereq(Remove(hash))

  def removeDowns(migHash: Seq[Byte]): EFreeDbAction[Unit] = ereq(RemoveDowns(migHash))

  def applyScript(script: Script, direction: Direction): EFreeDbAction[Unit] = ereq(ApplyScript(script, direction))

  def failure(f: String): EFreeDbAction[Nothing] = ereq(Failure(f))

  type WFreeDbAction[+A] = WriterT[FreeDbAction, UpMoveState, A]
  def WFreeDbAction[A](t: FreeDbAction[(UpMoveState, A)]) = WriterT[FreeDbAction, UpMoveState, A](t)

  type EWFreeDbAction[+A] = EitherT[WFreeDbAction, String, A]
  def EWFreeDbAction[A](wa: WFreeDbAction[SE[A]]) = EitherT[WFreeDbAction, String, A](wa)

  type EA[+A] = Either[String, A]

  def eit[A](ea: EA[A]): SE[A] = ea match {
    case Left(s) => -\/(s)
    case Right(a) => \/-(a)
  }

  def idTransformation(db: Database): (DbAction ~> Id.Id) = new (DbAction ~> Id.Id) {
    def apply[T](a: DbAction[T]): T = a match {
      case State => eit(db.state).asInstanceOf[T]
      case Downs(hash) => eit(db.downs(hash)).asInstanceOf[T]
      case Add(migrationInfo) => eit(db.add(migrationInfo)._1.toLeft(())).asInstanceOf[T]
      case AddDowns(migHash, downs) => eit(db.addDowns(migHash, downs)._1.toLeft(())).asInstanceOf[T]
      case Remove(hash) => eit(db.remove(hash)._1.toLeft(())).asInstanceOf[T]
      case RemoveDowns(migHash) => eit(db.removeDowns(migHash)._1.toLeft(())).asInstanceOf[T]
      case ApplyScript(script, direction) => eit(db.applyScript(script, direction)._1.toLeft(())).asInstanceOf[T]
      case Failure(f) => eit(Left(f)).asInstanceOf[T]
    }
  }

  def ffIdTransformation(db: Database) = freeLift(idTransformation(db))

  type FID[+A] = () => A

  def fidTransformation(db: Database): (DbAction ~> FID) = new (DbAction ~> FID) {
    def apply[T](a: DbAction[T]): FID[T] = () => idTransformation(db)[T](a)
  }

  def ffFidTransformation(db: Database) = freeLift(fidTransformation(db))

  def extractFid[A](fid: FID[Free[FID, A]]): Free[FID, A] = fid()

  def run[A](fba: EFreeDbAction[A])(db: Database): SE[A] = fba.run.mapSuspension(ffFidTransformation(db)).run
}
