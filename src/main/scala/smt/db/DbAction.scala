package smt.db

import smt.util.ActionTypes

object DbAction extends ActionTypes[Database] {

  def connection(): EDKleisli[Connection] = EDKleisli(_.connection())
}
