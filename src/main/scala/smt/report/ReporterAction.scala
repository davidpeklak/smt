package smt.report

import smt.NamedMoveStates
import smt.util.ActionTypes


object ReporterAction extends ActionTypes[Reporter] {
  def report(nms: NamedMoveStates): DKleisli[Unit] = DKleisli(_.report(nms).getOrElse(()))
}
