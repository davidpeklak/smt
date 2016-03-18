package smt.report

import smt.NamedMoveStates

object ReportersAction {

  def reportToAll(nms: NamedMoveStates)(reporters: List[Reporter]): Unit = {
    reporters.foreach(_.report(nms))
  }
}
