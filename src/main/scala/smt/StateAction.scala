package smt

case class StateAction[S, +A](run: S => (A, S)) {

  import StateAction._

  def flatMap[B](f: A => StateAction[S, B]): StateAction[S, B] = StateAction(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map[B](f: A => B): StateAction[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](that: StateAction[S, B])(f: (A, B) => C): StateAction[S, C] = flatMap(a => that.map(b => f(a, b)))
}

object StateAction {
  def unit[S, B](b: B): StateAction[S, B] = StateAction(s => (b, s))

  def get[S]: StateAction[S, S] = StateAction(s => (s, s))

  def set[S](s: S): StateAction[S, Unit] = StateAction(_ => ((), s))

  def modify[S](f: S => S): StateAction[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // def sequence[S](fs: Seq[StateAction[S, Unit]]): StateAction[S, Unit] =
  //   fs.foldRight(unit[S, Seq[Unit]](Nil))((f, acc) => f.map2(acc)(_ +: _)).flatMap( _ => unit())

  def sequence[S, A](fs: Seq[StateAction[S, A]]): StateAction[S, Seq[A]] =
    fs.foldRight(unit[S, Seq[A]](Nil))((f, acc) => f.map2(acc)(_ +: _))
}
