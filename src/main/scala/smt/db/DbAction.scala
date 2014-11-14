package smt.db

import smt.util.ActionTypes
import smt.describe.DescribeAction

object DbAction {
  type HasDb[α] = α => Database
}

trait DbAction[T] extends ActionTypes[T] {

  val hasDb: DbAction.HasDb[T]
  val hasLogger: DescribeAction.HasLogger[T]

  def connection(): EDKleisli[Connection] = EDKleisli(hasDb(_).connection())
}
