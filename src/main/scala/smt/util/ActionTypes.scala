package smt.util

import scalaz._
import scalaz.concurrent.Future
import scalaz.\/-
import smt.{NamedMoveStates, DownMoveState, UpMoveState}

trait ActionTypes[D] {

  type SE[A] = (String \/ A)

  type DKleisli[+A] = Kleisli[Future, D, A]

  def DKleisli[A](f: D => A): DKleisli[A] = Kleisli[Future, D, A](D => Future.delay(f(D)))

  type EDKleisli[+A] = EitherT[DKleisli, String, A]

  def EDKleisli[A](a: DKleisli[SE[A]]): EDKleisli[A] = EitherT[DKleisli, String, A](a)

  def EDKleisli[A](f: D => SE[A]): EDKleisli[A] = EDKleisli(DKleisli(f))

  def point[A](a: A): DKleisli[A] = DKleisli(_ => a)

  def ePoint[A](a: A): EDKleisli[A] = EDKleisli(DKleisli(_ => \/-(a)))

  def ask: DKleisli[D] = Kleisli.ask[Future, D]

  def eAsk: EDKleisli[D] = EDKleisli(ask.map(\/-(_)))

  def failure(f: String): EDKleisli[Nothing] = EDKleisli(DKleisli(_ => -\/(f)))

  trait WriterTypes[W] {

    implicit val wMonoid: Monoid[W]

    type WDKleisli[+A] = WriterT[DKleisli, W, A]

    def WDKleisli[A](a: DKleisli[(W, A)]): WDKleisli[A] = WriterT[DKleisli, W, A](a)

    type EWDKleisli[+A] = EitherT[WDKleisli, String, A]

    def EWDKleisli[A](wa: WDKleisli[SE[A]]) = EitherT[WDKleisli, String, A](wa)

    def lift[A](dKleisli: DKleisli[A]): WDKleisli[A] = WriterT.writerTMonadTrans[W].liftM(dKleisli)

    def liftE[A](edKleisli: EDKleisli[A]): EWDKleisli[A] = EWDKleisli(lift(edKleisli.run))

    def putE[A](edKleisli: EDKleisli[A])(w: W): EWDKleisli[A] = EitherT[WDKleisli, String, A](WriterT.put(edKleisli.run)(w))

    lazy val EWSyntax = EitherTWriterT.eitherTWriterTSyntax[DKleisli, String, W]

    lazy val ewSyntax = KleisliStack.EitherTWriterTKleisli[String, W].tKleisliSyntax[Future, D]

    // the implicits are lazy, otherwise I get NullPointerExceptions
    implicit lazy val wInstance = WriterT.writerTMonad[DKleisli, W]

    implicit lazy val ewInstance = EitherT.eitherTMonad[WDKleisli, String]

    implicit lazy val wUnstackedInstance = WriterT.writerTMonad[Future, W]

    implicit lazy val ewUnstackedInstance = EitherT.eitherTMonad[({type λ[+α] = WriterT[Future, W, α]})#λ, String]
  }

  def writerTypes[S : Monoid]: WriterTypes[S] = new WriterTypes[S] {
    lazy val wMonoid = implicitly[Monoid[S]]
  }

  val upMoveTypes = writerTypes[UpMoveState]

  val downMoveTypes = writerTypes[DownMoveState]

  val namedMoveTypes = writerTypes[NamedMoveStates]

  val ekSyntax = KleisliStack.EitherTKleisli[String].tKleisliSyntax[Future, D]

  val eSyntax = EitherTHaerte.eitherTSyntax[DKleisli, String]
}
