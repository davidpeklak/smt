package smt

import scalaz._
import scalaz.std.list._
import scalaz.Free.Return

object FreeDbAction {
  
  import DbAction._

  import FreeFunctor._

  type DbActionFreeFunctor[+A] = FreeFunctor[DbAction, A]

  type FreeDbAction[+A] = Free[DbActionFreeFunctor, A]

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
  
  def eaTransformation(db: Database): (DbAction ~> EA) = new (DbAction ~> EA) {
    def apply[T](a: DbAction[T]): EA[T] =  a match {
      case State => db.state
      case Downs(hash) => db.downs(hash).asInstanceOf[EA[T]]
      case Add(migrationInfo) => db.add(migrationInfo)._1.toLeft(()).asInstanceOf[EA[T]]
      case AddDowns(migHash, downs) => db.addDowns(migHash, downs)._1.toLeft(()).asInstanceOf[EA[T]]
      case Remove(hash) => db.remove(hash)._1.toLeft(()).asInstanceOf[EA[T]]
      case RemoveDowns(migHash) => db.removeDowns(migHash)._1.toLeft(()).asInstanceOf[EA[T]]
      case ApplyScript(script, direction) => db.applyScript(script, direction)._1.toLeft(()).asInstanceOf[EA[T]]
      case TryApplyScript(script, direction) => Right(db.applyScript(script, direction)._1).asInstanceOf[EA[T]]
      case Failure(f) => Left(f)
    }
  }

  implicit val eaFunctor: Functor[EA] = new Functor[EA]  {
    def map[A, B](a: EA[A])(f: A => B): EA[B] = a.right.map(f)
  }

  def ffEaTransformation(db: Database) = freeLift(eaTransformation(db))

  def extractEither[A](ea: EA[Free[EA, A]]): Free[EA, A] = ea match {
    case Right(fea) => fea
    case Left(l) => throw new Exception("Scheisse")
  }

  // all the FEA shit is senseless
  type FEA[+A] = () => EA[A]

  def feaTransformation(db: Database): (DbAction ~> FEA) = new (DbAction ~> FEA) {
    def apply[T](a: DbAction[T]): FEA[T] = () => eaTransformation(db)(a)
  }

  implicit val feaFunctor: Functor[FEA] = new Functor[FEA]  {
    def map[A, B](a: FEA[A])(f: A => B): FEA[B] = () => {
      val ea = a()
      ea.right.map(f)
    }
  }
  
  def ffFeaTransformation(db: Database) = freeLift(feaTransformation(db))

  def extractFEither[A](fea: FEA[Free[FEA, A]]): Free[FEA, A] = fea() match {
    case Right(ffea) => ffea
    case Left(l) => throw new Exception("Scheisse")
  }

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

  def run[A](fdba: FreeDbAction[A], db: Database): Either[String, A] = fdba.foldMap(ffDbkTransformation).run(db)

}
