package smt.db

import smt.util.ActionTypes

object DbAction {
  type HasDb[α] = α => Database
}

trait DbAction[T] extends ActionTypes[T] {

  val hasDb: DbAction.HasDb[T]

  def connection(): EDKleisli[Connection] = EDKleisli(hasDb(_).connection())
}
