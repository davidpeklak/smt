package smt

import scalaz.{~>, Free, Functor}
import scalaz.Free.{Return, Suspend}


sealed trait FreeFunctor[F[_], +A] {
  def map[B](f: A => B): FreeFunctor[F, B]
}

case class FFMap[F[_], I, +A](fa: F[I], g: I => A) extends FreeFunctor[F, A] {
  def map[B](f: A => B) = FFMap(fa, g andThen f)
}

object FreeFunctor {
  implicit def freeFunctorFunctor[F[_]]: Functor[({type λ[A] = FreeFunctor[F, A]})#λ] = new Functor[({type λ[A] = FreeFunctor[F, A]})#λ] {
    def map[A, B](fa: FreeFunctor[F, A])(f: A => B): FreeFunctor[F, B] = fa map f
  }

  type FreeC[F[_], A] = Free[({type λ[+x] = FreeFunctor[F, x]})#λ, A]

  def request[F[_], A](fa: F[A]): FreeC[F, A] = {
    implicit val freeFunctorFunctorF = freeFunctorFunctor[F]
    Suspend[({type λ[+x] = FreeFunctor[F, x]})#λ, A](FFMap(fa, (a: A) => Return[({type λ[+x] = FreeFunctor[F, x]})#λ, A](a)))
  }

  def freeLift[F[_], G[_]](fg: F ~> G)(implicit G: Functor[G]): ({type f[x] = FreeFunctor[F, x]})#f ~> G = new (({type f[x] = FreeFunctor[F, x]})#f ~> G) {
    def apply[A](f: FreeFunctor[F, A]): G[A] = f match {
      case FFMap(fa, g) => G.map(fg(fa))(g)
    }
  }
}
