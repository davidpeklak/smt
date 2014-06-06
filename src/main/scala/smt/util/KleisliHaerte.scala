package smt.util

import scalaz.{Bind, Kleisli}
import scalaz.syntax.Ops
import scalaz.Kleisli._


object KleisliHaerte {

  trait KleisliOps[M[+ _],A, B] extends Ops[Kleisli[M, A, B]] {

    trait Combining[AB] {
      val bc: Bind[M]
      val ccc: (A, B) => AB
      def !=>[C](k: Kleisli[M, AB, C]): Kleisli[M, A, C] =  kleisli((a: A) => bc.bind(self(a))(b => k(ccc(a, b))))
    }

    def >=!>[AB, C](cc: (A, B) => AB)(k: Kleisli[M, AB, C])(implicit b: Bind[M]): Kleisli[M, A, C] =  kleisli((a: A) => b.bind(self(a))(b => k(cc(a, b))))
    def >=![AB](cc: (A, B) => AB)(implicit b: Bind[M]): Combining[AB] = new Combining[AB] {
      val bc = b
      val ccc = cc
    }
  }

  trait KleisliSyntax[M[+ _], A] {
    implicit def toKleisliOps[B](v: Kleisli[M, A, B]): KleisliOps[M, A, B] = new KleisliOps[M, A, B] {
      val self = v
    }
  }

  def kleisliSyntax[M[+ _], A]: KleisliSyntax[M, A] = new KleisliSyntax[M, A] {}
}
