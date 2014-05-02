package smt.util

import scalaz._
import scalaz.concurrent.Future
import scalaz.\/-

trait ActionTypes[D] {

  type SE[A] = (String \/ A)

  type DKleisli[+A] = Kleisli[Future, D, A]

  def DKleisli[A](f: D => A): DKleisli[A] = Kleisli[Future, D, A](D => Future.delay(f(D)))

  type EDKleisli[+A] = EitherT[DKleisli, String, A]

  // implicit val EDM = Kleisli.kleisliMonadPlus

  def EDKleisli[A](a: DKleisli[SE[A]]) = EitherT[DKleisli, String, A](a)

  def point[A](a: A): DKleisli[A] = DKleisli(_ => a)

  def ePoint[A](a: A): EDKleisli[A] = EDKleisli(DKleisli(_ => \/-(a)))

  def failure(f: String): EDKleisli[Nothing] = EDKleisli(DKleisli(_ => -\/(f)))

  trait WriterTypes[W] {

    type WDKleisli[+A] = WriterT[DKleisli, W, A]

    def WDKleisli[A](a: DKleisli[(W, A)]): WDKleisli[A] = WriterT[DKleisli, W, A](a)

    type EWDKleisli[+A] = EitherT[WDKleisli, String, A]

    def EWDKleisli[A](wa: WDKleisli[SE[A]]) = EitherT[WDKleisli, String, A](wa)

    def lift[A](dKleisli: DKleisli[A])(implicit W: Monoid[W]): WDKleisli[A] = WriterT.writerTMonadTrans[W].liftM(dKleisli)

    def liftE[A](edKleisli: EDKleisli[A])(implicit W: Monoid[W]): EWDKleisli[A] = EWDKleisli(lift(edKleisli.run))

    def putE[A](edKleisli: EDKleisli[A])(w: W): EWDKleisli[A] = EitherT[WDKleisli, String, A](WriterT.put(edKleisli.run)(w))

    val EWSyntax = EitherTWriterT.eitherTWriterTSyntax[DKleisli, String, W]
  }

  def writerTypes[S]: WriterTypes[S] = new WriterTypes[S] {}
}
