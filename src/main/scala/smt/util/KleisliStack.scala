package smt.util

import scalaz.syntax.Ops
import scalaz._
import Kleisli._

object KleisliStack {

  /**
   * a typeclass that provides functions to stack a monad-transformer
   * and to unstack a monad-transformer
   * Data: the value type that the transformer expects
   * @tparam Data the expected data type of the monad transformer
   * @tparam Stacked the type that results when applying the monad transformer to a monad
   */
  trait TStacking[Data[+ _], Stacked[_[+ _], + _]] {

    def stack[M[+ _], A](m: M[Data[A]]): Stacked[M, A]

    def unstack[M[+ _], A](s: Stacked[M, A]): M[Data[A]]
  }

  class TKleisli[Data[+ _], Stacked[_[+ _], + _]](S: TStacking[Data, Stacked]) {

    type TKleisli[M[+ _], D, +A] = Stacked[({type λ[+α] = Kleisli[M, D, α]})#λ, A]

    trait TKleisliOps[M[+ _], D, A] extends Ops[TKleisli[M, D, A]] {

      tKlseisliOps =>

      def runE: D => Stacked[M, A] = (d: D) => S.stack(S.unstack[({type λ[+α] = Kleisli[M, D, α]})#λ, A](self)(d))

      def swapStack: Kleisli[({type λ[+α] = Stacked[M, α]})#λ, D, A] = {
        val un: Kleisli[M, D, Data[A]] = S.unstack[({type λ[+α] = Kleisli[M, D, α]})#λ, A](self)
        un.mapK[({type λ[+α] = Stacked[M, α]})#λ, A]((m: M[Data[A]]) => S.stack(m))
      }

      def swapStackBack[B](swapped: Kleisli[({type λ[+α] = Stacked[M, α]})#λ, D, B]): TKleisli[M, D, B] = {
        val un: Kleisli[M, D, Data[B]] = swapped.mapK[M, Data[B]]((m: Stacked[M, B]) => S.unstack(m))
        S.stack[({type λ[+α] = Kleisli[M, D, α]})#λ, B](un)
      }

      def >=>[B](k: TKleisli[M, A, B])(implicit eb: Bind[({type λ[+α] = Stacked[M, α]})#λ]): TKleisli[M, D, B] = {
        val syn = tKleisliSyntax[M, A]
        import syn._

        swapStackBack(this.swapStack >=> k.swapStack)
      }

      trait Combining[DA] {
        val bc: Bind[({type λ[+α] = Stacked[M, α]})#λ]
        val ccc: (D, A) => DA

        def !=>[C](k: TKleisli[M, DA, C]): TKleisli[M, D, C] = {
          val syn = tKleisliSyntax[M, DA]
          import syn._

          val kl: Kleisli[({type λ[+α] = Stacked[M, α]})#λ, D, A] = tKlseisliOps.swapStack
          val klc = k.swapStack
          val kl2: Kleisli[({type λ[+α] = Stacked[M, α]})#λ, D, C] = kleisli[({type λ[+α] = Stacked[M, α]})#λ, D, C]((a: D) => bc.bind(kl.run(a))(b => klc.run(ccc(a, b))))
          swapStackBack(kl2)
        }
      }

      def >=![DA](cc: (D, A) => DA)(implicit b: Bind[({type λ[+α] = Stacked[M, α]})#λ]): Combining[DA] = new Combining[DA] {
        val bc = b
        val ccc = cc
      }
    }

    trait TKleisliSyntax[M[+ _], D] {
      implicit def toTKleisliOps[A](v: TKleisli[M, D, A]): TKleisliOps[M, D, A] = new TKleisliOps[M, D, A] {
        val self = v
      }
    }

    def tKleisliSyntax[M[+ _], D]: TKleisliSyntax[M, D] = new TKleisliSyntax[M, D] {}
  }

  trait EitherTStackingTypes[E] {
    type Data[+A] = E \/ A

    type Stacked[M[+ _], +A] = EitherT[M, E, A]
  }

  def EitherTStacking[E]: TStacking[EitherTStackingTypes[E]#Data, EitherTStackingTypes[E]#Stacked] = new TStacking[EitherTStackingTypes[E]#Data, EitherTStackingTypes[E]#Stacked] {

    def stack[M[+ _], A](m: M[EitherTStackingTypes[E]#Data[A]]): EitherTStackingTypes[E]#Stacked[M, A] = EitherT[M, E, A](m)

    def unstack[M[+ _], A](s: EitherTStackingTypes[E]#Stacked[M, A]): M[EitherTStackingTypes[E]#Data[A]] = s.run
  }

  def EitherTKleisli[E] = new TKleisli[EitherTStackingTypes[E]#Data, EitherTStackingTypes[E]#Stacked](EitherTStacking[E])

  trait WriterTStackingTypes[W] {
    type Data[+A] = (W, A)

    type Stacked[M[+ _], +A] = WriterT[M, W, A]
  }

  def WriterTStacking[W]: TStacking[WriterTStackingTypes[W]#Data, WriterTStackingTypes[W]#Stacked] = new TStacking[WriterTStackingTypes[W]#Data, WriterTStackingTypes[W]#Stacked] {

    def stack[M[+ _], A](m: M[WriterTStackingTypes[W]#Data[A]]): WriterTStackingTypes[W]#Stacked[M, A] = WriterT[M, W, A](m)

    def unstack[M[+ _], A](s: WriterTStackingTypes[W]#Stacked[M, A]): M[WriterTStackingTypes[W]#Data[A]] = s.run
  }

  def WriterTKleisli[W] = new TKleisli[WriterTStackingTypes[W]#Data, WriterTStackingTypes[W]#Stacked](WriterTStacking[W])

  trait EitherTWriterTStackingTypes[E, W] {
    type Data[+A] = (W, E \/ A)

    type Stacked[M[+ _], +A] = EitherT[({type λ[+α] = WriterT[M, W, α]})#λ, E, A]
  }

  def EitherTWriterTStacking[E, W]: TStacking[EitherTWriterTStackingTypes[E, W]#Data, EitherTWriterTStackingTypes[E, W]#Stacked] = new TStacking[EitherTWriterTStackingTypes[E, W]#Data, EitherTWriterTStackingTypes[E, W]#Stacked] {

    def stack[M[+ _], A](m: M[EitherTWriterTStackingTypes[E, W]#Data[A]]): EitherTWriterTStackingTypes[E, W]#Stacked[M, A] = EitherT[({type λ[+α] = WriterT[M, W, α]})#λ, E, A](WriterT[M, W, E \/ A](m))

    def unstack[M[+ _], A](s: EitherTWriterTStackingTypes[E, W]#Stacked[M, A]): M[EitherTWriterTStackingTypes[E, W]#Data[A]] = s.run.run
  }

  def EitherTWriterTKleisli[E, W] = new TKleisli[EitherTWriterTStackingTypes[E, W]#Data, EitherTWriterTStackingTypes[E, W]#Stacked](EitherTWriterTStacking[E, W])
}
