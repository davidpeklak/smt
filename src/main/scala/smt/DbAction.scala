package smt

case class DbAction[A](s: StateAction[Database, Either[String, A]]) {

  import DbAction.EA

  def apply(db: Database): EA[A] = s.run(db)._1

  def map[B](f: A => B): DbAction[B] = DbAction(s.map(_.right.map(f)))

  def flatMap[B](f: A => DbAction[B]): DbAction[B] = DbAction(s.flatMap(ea => ea match {
    case Right(a) => f(a).s
    case Left(b) => StateAction.unit(Left(b))
  }))

  def map2[B, C](that: DbAction[B])(f: (A, B) => C): DbAction[C] = flatMap(a => that.map(b => f(a, b)))
}

object DbAction {
  type EA[A] = Either[String, A]

  type DbStateAction[A] = StateAction[Database, EA[A]]

  type R = (Option[String], Database)

  def unit[A](a: => A) = DbAction(StateAction.unit(Right(a)))

  def sequence[S, A](fs: Seq[DbAction[A]]): DbAction[Seq[A]] =
    fs.foldRight(unit[Seq[A]](Nil))((f, acc) => f.map2(acc)(_ +: _))

  def get: DbAction[Database] = DbAction(StateAction.get[Database].map(a => Right(a)))

  def set(r: R): DbAction[Unit] = DbAction(StateAction.set(r._2).map(_ => r._1.toLeft(())))

  def modify(f: Database => (Option[String], Database)): DbAction[Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()


  def getFromDb[A](f: Database => EA[A]): DbAction[A] = DbAction(get.s.map(_.right.flatMap(f)))

  def state: DbAction[Seq[MigrationInfo]] = getFromDb(_.state)

  def downs(hash: Seq[Byte]): DbAction[Seq[Script]] = getFromDb(_.downs(hash))

  def add(migrationInfo: MigrationInfo): DbAction[Unit] = modify(_.add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): DbAction[Unit] = modify(_.addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): DbAction[Unit] = modify(_.remove(hash))

  def removeDowns(migHash: Seq[Byte]): DbAction[Unit] = modify(_.removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): DbAction[Unit] = modify(_.applyScript(script, direction: Direction))
}

