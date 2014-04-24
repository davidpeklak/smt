package smt

import scalaz.syntax.Ops
import scalaz.{Traverse, Applicative}

trait TraverseStackSafeOps[F[_], A] extends Ops[F[A]] {
  def F: Traverse[F]

  final def traverse__[G[_]](f: A => G[Unit])(implicit G: Applicative[G]): G[Unit] = {
    val t = G.traverse(self)(f)(F)
    G.map(t)(_ => ())
  }
}

trait TraverseStackSafeSyntax[F[_]] {
  implicit def ToTraverseStackSafeOps[A](v: F[A])(implicit FT: Traverse[F]): TraverseStackSafeOps[F, A] = new TraverseStackSafeOps[F, A] {
    def self = v
    def F = FT
  }
}

object TraverseStackSafeSyntax extends
TraverseStackSafeSyntax[List]
