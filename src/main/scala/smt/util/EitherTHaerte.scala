package smt.util

import scalaz.syntax.Ops
import scalaz._
import scalaz.-\/

object EitherTHaerte {

  trait EitherTOps[F[+ _], A, B] extends Ops[EitherT[F, A, B]] {
    def andFinally(f: EitherT[F, A, Unit])(implicit F: Bind[F], A: Semigroup[A]): EitherT[F, A, B] = {
      EitherT(
        F.bind(self.run)(r1 => F.map(f.run)(r2 => {
          (r1, r2) match {
            case (-\/(a), -\/(ra)) => -\/(A.append(a, ra))
            case (-\/(a),\/-(_)) => -\/(a)
            case (\/-(b), -\/(ra)) => -\/(ra)
            case (\/-(b), \/-(_)) => \/-(b)
          }
        }))
      )
    }
  }

  trait EitherTSyntax[F[+ _], A] {
    implicit def toEitherTOps[B](v: EitherT[F, A, B]): EitherTOps[F, A, B] = new EitherTOps[F, A, B] {
      val self = v
    }
  }

  def eitherTSyntax[F[+ _], A]: EitherTSyntax[F, A] = new EitherTSyntax[F, A] {}


}
