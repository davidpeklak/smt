package smt.util

import scalaz.syntax.Ops
import scalaz._
import scalaz.-\/
import scalaz.Scalaz._

object EitherHaerte {

  trait EitherOps[B] extends Ops[String \/ B] {
    def andFinally(f: => String \/ Unit): String \/ B = {
      (self, f) match {
        case (-\/(a), -\/(ra)) => -\/(a + ra)
        case (-\/(a), \/-(_)) => -\/(a)
        case (\/-(b), -\/(ra)) => -\/(ra)
        case (\/-(b), \/-(_)) => \/-(b)
      }
    }

    def onLeftDo(f: String => String \/ Unit): String \/ B = {
      self match {
        case err@ -\/(e) => f(e) >> err
        case b@ \/-(_) => b
      }
    }

    /*def recover[BB >: B](r: (W, A) => EitherTWriterT[F, A, BB, W])(implicit F: Monad[F], W: Semigroup[W]): EitherTWriterT[F, A, BB, W] = {
      val f: F[(W, A \/ BB)] = F.bind(self.run.run) {
        case (w, -\/(a)) => F.map(r(w, a).run.run)(wabb2 => (W.append(w, wabb2._1), wabb2._2))
        case (w, rb@ \/-(_)) => F.point((w, rb))
      }

      EitherT[({type λ[+α] = WriterT[F, W, α]})#λ, A, BB](WriterT[F, W, A \/ BB](f))
    }*/
  }

  object EitherSyntax {
    implicit def toEitherOps[B](v: String \/ B): EitherOps[B] = new EitherOps[B] {
      val self = v
    }
  }
}
