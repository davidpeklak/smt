package smt.report

import smt.NamedMoveStates
import scalaz.\/

trait Reporter {

  def report(s: NamedMoveStates): String \/ Unit
}
