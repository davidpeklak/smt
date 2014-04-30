package smt

import scalaz.syntax.Ops
import scalaz._

object EitherTWriterT {

  type EitherTWriterT[F[+ _], A, B, W] = EitherT[({type λ[+α] = WriterT[F, W, α]})#λ, A, B]

  trait EitherTWriterTOps[F[+ _], A, B, W] extends Ops[EitherTWriterT[F, A, B, W]] {
    def :\/++>[WW >: W](lw: => WW, rw: => WW)(implicit F: Functor[F], W: Semigroup[WW]): EitherTWriterT[F, A, B, WW] = {
      EitherT[({type λ[+α] = WriterT[F, WW, α]})#λ, A, B](self.run :++>> {
        case -\/(_) => lw
        case \/-(_) => rw
      })
    }

    def :\/-++>[WW >: W](w: => WW)(implicit F: Functor[F], W: Monoid[WW]): EitherTWriterT[F, A, B, WW] = {
      EitherT[({type λ[+α] = WriterT[F, WW, α]})#λ, A, B](self.run :++>> {
        case -\/(_) => W.zero
        case \/-(_) => w
      })
    }

    def :-\/++>[WW >: W](w: => WW)(implicit F: Functor[F], W: Monoid[WW]): EitherTWriterT[F, A, B, WW] = {
      EitherT[({type λ[+α] = WriterT[F, WW, α]})#λ, A, B](self.run :++>> {
        case -\/(_) => w
        case \/-(_) => W.zero
      })
    }

    def :++>>[WW >: W](f: B => WW)(implicit F: Functor[F], W: Monoid[WW]): EitherTWriterT[F, A, B, WW] =
      EitherT[({type λ[+α] = WriterT[F, WW, α]})#λ, A, B](self.run :++>> {
        case -\/(_) => W.zero
        case \/-(b) => f(b)
      })

    def recover[BB >: B](r: (W, A) => EitherTWriterT[F, A, BB, W])(implicit F: Monad[F], W: Semigroup[W]): EitherTWriterT[F, A, BB, W] = {
      val f: F[(W, A \/ BB)] = F.bind(self.run.run) {
        case (w, -\/(a)) => F.map(r(w, a).run.run)(wabb2 => (W.append(w, wabb2._1), wabb2._2))
        case (w, rb @ \/-(_)) => F.point((w, rb))
      }

      EitherT[({type λ[+α] = WriterT[F, W, α]})#λ, A, BB](WriterT[F, W, A \/ BB](f))
    }

    def conclude[BB >: B](r: (W, A) => EitherTWriterT[F, A, BB, W])(c: (W, B) => EitherTWriterT[F, A, BB, W])(implicit F: Monad[F], W: Semigroup[W]): EitherTWriterT[F, A, BB, W] = {
      val f: F[(W, A \/ BB)] = F.bind(self.run.run) {
        case (w, -\/(a)) => F.map(r(w, a).run.run)(wabb2 => (W.append(w, wabb2._1), wabb2._2))
        case (w, \/-(b)) => F.map(c(w, b).run.run)(wabb2 => (W.append(w, wabb2._1), wabb2._2))
      }

      EitherT[({type λ[+α] = WriterT[F, W, α]})#λ, A, BB](WriterT[F, W, A \/ BB](f))
    }

    def mapWritten[X](f: W => X)(implicit F: Functor[F]): EitherTWriterT[F, A, B, X] = {
      EitherT[({type λ[+α] = WriterT[F, X, α]})#λ, A, B](self.run.mapWritten(f))
    }
  }

  trait EitherTWriterTSyntax[F[+ _], A, W] {
    implicit def toEitherTWriterTOps[B](v: EitherTWriterT[F, A, B, W]): EitherTWriterTOps[F, A, B, W] = new EitherTWriterTOps[F, A, B, W] {
      val self = v
    }
  }

  def eitherTWriterTSyntax[F[+ _], A, W]: EitherTWriterTSyntax[F, A, W] = new EitherTWriterTSyntax[F, A, W] {}
}
