package smt.report

import scalaz._
import scalaz.concurrent.Future
import smt.NamedMoveStates


object ReporterAction {

  type RpKleisli[+A] = Kleisli[Future, Reporter, A]

  def RpKleisli[A](f: Reporter => A): RpKleisli[A] = Kleisli[Future, Reporter, A](db => Future.delay(f(db)))

  def report(nms: NamedMoveStates): RpKleisli[Unit] = RpKleisli(_.report(nms).getOrElse(()))
}
