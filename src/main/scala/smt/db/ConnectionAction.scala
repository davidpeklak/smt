package smt.db

import smt.migration.{Test, Direction, Script, MigrationInfo}
import smt.util.ActionTypes

trait HasConnection {
  val connection: Connection
}

case class HasConnectionOnly (connection: Connection) extends HasConnection

trait ConnectionAction[T <: HasConnection] extends ActionTypes[T] {

  def init(): EDKleisli[Unit] = EDKleisli(_.connection.init())

  def state(): EDKleisli[Seq[MigrationInfo]] = EDKleisli(_.connection.state)

  def downs(hash: Seq[Byte]): EDKleisli[Seq[Script]] = EDKleisli(_.connection.downs(hash))

  def add(migrationInfo: MigrationInfo): EDKleisli[Unit] = EDKleisli(_.connection.add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EDKleisli[Unit] = EDKleisli(_.connection.addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(_.connection.remove(hash))

  def removeDowns(migHash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(_.connection.removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): EDKleisli[Unit] = EDKleisli(_.connection.applyScript(script, direction))

  def doTest(test: Test): EDKleisli[Unit] = EDKleisli(t => test.run(t.connection))

  def close(): EDKleisli[Unit] = EDKleisli(_.connection.close())
}
