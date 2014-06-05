package smt.db

import smt.util.ActionTypes

trait HasDb {
  val db: Database
}

case class HasDbOnly(db: Database) extends HasDb

trait DbAction[T <: HasDb] extends ActionTypes[T] {

  def connection(): EDKleisli[Connection] = EDKleisli(_.db.connection())
}
