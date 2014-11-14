package smt.db

import smt.migration.{Test, Direction, Script, MigrationInfo}
import smt.util.ActionTypes
import java.util.Date
import smt.describe.DescribeAction

object ConnectionAction {
  type HasConnection[α] = α => Connection
}

trait ConnectionAction[T] extends ActionTypes[T] {

  val hasConnection: ConnectionAction.HasConnection[T]
  val hasLogger: DescribeAction.HasLogger[T]

  def init(): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).init(hasLogger(d)))

  def state(): EDKleisli[Seq[MigrationInfo]] = EDKleisli(d => hasConnection(d).state(hasLogger(d)))

  def downs(hash: Seq[Byte]): EDKleisli[Seq[Script]] = EDKleisli(d => hasConnection(d).downs(hasLogger(d))(hash))

  def add(migrationInfo: MigrationInfo): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).add(hasLogger(d))(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).addDowns(hasLogger(d))(migHash, downs))

  def remove(hash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).remove(hasLogger(d))(hash))

  def removeDowns(migHash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).removeDowns(hasLogger(d))(migHash))

  def applyScript(script: Script, direction: Direction): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).applyScript(hasLogger(d))(script, direction))

  def doTest(test: Test): EDKleisli[Unit] = EDKleisli(d => test.run(hasConnection(d))(hasLogger(d)))

  def close(): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).close(hasLogger(d))())
}

object AddAction {
  type HasUser[α] = α => String

  type HasRemark[α] = α => Option[String]
}

trait AddAction[T] extends ConnectionAction[T] {
  val hasUser: AddAction.HasUser[T]
  val hasRemark: AddAction.HasRemark[T]

  def add(name: String, hash: Seq[Byte], dateTime: Date): EDKleisli[Unit] = EDKleisli(d => hasConnection(d).add(hasLogger(d))(new MigrationInfo(name, hash, dateTime, Some(hasUser(d)), hasRemark(d))))
}