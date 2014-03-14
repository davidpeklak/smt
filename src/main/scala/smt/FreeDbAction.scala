package smt

import scalaz.{Monad, Kleisli, Free, ~>}
import scalaz.std.list._

object FreeDbAction {
  
  import DbAction._

  import FreeFunctor._

  type DbActionFreeFunctor[+A] = FreeFunctor[DbAction, A]

  type FreeDbAction[A] = Free[DbActionFreeFunctor, A]

  val FDBAM = Free.freeMonad[DbActionFreeFunctor]

  private def req[A](dba: DbAction[A]): FreeDbAction[A] = request[DbAction, A](dba)

  def sequence[A](ss: Seq[FreeDbAction[A]]): FreeDbAction[Seq[A]] = FDBAM.sequence(ss.toList)

  def state: FreeDbAction[Seq[MigrationInfo]] = req(State)

  def downs(hash: Seq[Byte]): FreeDbAction[Seq[Script]] = req(Downs(hash))

  def add(migrationInfo: MigrationInfo) : FreeDbAction[Unit] = req(Add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): FreeDbAction[Unit] = req(AddDowns(migHash, downs))

  def remove(hash: Seq[Byte]): FreeDbAction[Unit] = req(Remove(hash))

  def removeDowns(migHash: Seq[Byte]): FreeDbAction[Unit] = req(RemoveDowns(migHash))

  def applyScript(script: Script, direction: Direction): FreeDbAction[Unit] = req(ApplyScript(script, direction))

  def tryApplyScript(script: Script, direction: Direction): FreeDbAction[Option[String]] = req(TryApplyScript(script, direction))

  def failure(f: String): FreeDbAction[Nothing] = req(Failure(f))

  type EA[+A] = Either[String, A]
  type DBK[+A] = Kleisli[EA, Database, A]

  def dbk[A](f: Database => EA[A]): DBK[A] = Kleisli[EA, Database, A](f)

  // this works under the assumption that the db implementation is stateless (i.e. all state is on the physical DB)
  val dbkTransformation: (DbAction ~> DBK) = new (DbAction ~> DBK){
    def apply[T](a: DbAction[T]): DBK[T] = a match {
      case State => dbk(db => db.state)
      case Downs(hash) => dbk(db => db.downs(hash).asInstanceOf[EA[T]])
      case Add(migrationInfo) => dbk(db => db.add(migrationInfo)._1.toLeft(()).asInstanceOf[EA[T]])
      case AddDowns(migHash, downs) => dbk(db => db.addDowns(migHash, downs)._1.toLeft(()).asInstanceOf[EA[T]])
      case Remove(hash) => dbk(db => db.remove(hash)._1.toLeft(()).asInstanceOf[EA[T]])
      case RemoveDowns(migHash) => dbk(db => db.removeDowns(migHash)._1.toLeft(()).asInstanceOf[EA[T]])
      case ApplyScript(script, direction) => dbk(db => db.applyScript(script, direction)._1.toLeft(()).asInstanceOf[EA[T]])
      case TryApplyScript(script, direction) => dbk(db => Right(db.applyScript(script, direction)._1).asInstanceOf[EA[T]])
      case Failure(f) => dbk(_ => Left(f))
    }
  }

  import scalaz.std.either._

  implicit val dbkMonad : Monad[DBK] = new Monad[DBK] {
    def point[A](a: => A): DBK[A] = dbk(_ => Right(a))

    def bind[A, B](fa: DBK[A])(f: A => DBK[B]): DBK[B] = fa.flatMap(f)
  }

  val ffDbkTransformation = freeLift(dbkTransformation)
}
