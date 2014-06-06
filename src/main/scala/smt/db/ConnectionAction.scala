package smt.db

import smt.migration.{Test, Direction, Script, MigrationInfo}
import smt.util.ActionTypes
import java.util.Date

object ConnectionAction {
  type HasConnection[α] = α => Connection
}

trait ConnectionAction[T] extends ActionTypes[T] {

  val hasConnection: ConnectionAction.HasConnection[T]

  def init(): EDKleisli[Unit] = EDKleisli(hasConnection(_).init())

  def state(): EDKleisli[Seq[MigrationInfo]] = EDKleisli(hasConnection(_).state)

  def downs(hash: Seq[Byte]): EDKleisli[Seq[Script]] = EDKleisli(hasConnection(_).downs(hash))

  def add(migrationInfo: MigrationInfo): EDKleisli[Unit] = EDKleisli(hasConnection(_).add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EDKleisli[Unit] = EDKleisli(hasConnection(_).addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(hasConnection(_).remove(hash))

  def removeDowns(migHash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(hasConnection(_).removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): EDKleisli[Unit] = EDKleisli(hasConnection(_).applyScript(script, direction))

  def doTest(test: Test): EDKleisli[Unit] = EDKleisli(t => test.run(hasConnection(t)))

  def close(): EDKleisli[Unit] = EDKleisli(hasConnection(_).close())
}

object AddAction {
  type HasUser[α] = α => String

  type HasRemark[α] = α => Option[String]
}

trait AddAction[T] extends ConnectionAction[T] {
  val hasUser: AddAction.HasUser[T]
  val hasRemark: AddAction.HasRemark[T]

  def add(name: String, hash: Seq[Byte], dateTime: Date): EDKleisli[Unit] = EDKleisli(t => hasConnection(t).add(new MigrationInfo(name, hash, dateTime, Some(hasUser(t)), hasRemark(t))))
}