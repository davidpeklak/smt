package smt.report

import smt.NamedMoveStates
import smt.util.ActionTypes
import scalaz.Kleisli
import scalaz.concurrent.Future
import smt.report.HasReporter.HasReporter
import scalaz.Scalaz._

object HasReporter {
  type HasReporter[α] = α => Reporter
}

trait ReporterAction[T] extends ActionTypes[T] {

  val hasReporter: HasReporter.HasReporter[T]

  def report(nms: NamedMoveStates): DKleisli[Unit] = DKleisli(hasReporter(_).report(nms).getOrElse(()))
}

object HasReporters {
  type HasReporters[α] = α => List[Reporter]
}

trait ReportersAction[T] extends ActionTypes[T] {

  val hasReporters: HasReporters.HasReporters[T]

  lazy val reporterAction = new ReporterAction[Reporter] {
    lazy val hasReporter: HasReporter[Reporter] = identity
  }

  def reportToAll(nms: NamedMoveStates): Kleisli[Future, T, Unit] = {
    Kleisli[Future, T, Unit](hasReporters(_).traverse_(reporterAction.report(nms).run))
  }
}
