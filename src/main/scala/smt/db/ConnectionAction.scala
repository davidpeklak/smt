package smt.db

import smt.migration.{Test, Direction, Script, MigrationInfo}
import smt.util.ActionTypes

object ConnectionAction extends ActionTypes[Connection] {

  def init(): EDKleisli[Unit] = EDKleisli(_.init())

  def state(): EDKleisli[Seq[MigrationInfo]] = EDKleisli(_.state)

  def downs(hash: Seq[Byte]): EDKleisli[Seq[Script]] = EDKleisli(_.downs(hash))

  def add(migrationInfo: MigrationInfo): EDKleisli[Unit] = EDKleisli(_.add(migrationInfo))

  def addDowns(migHash: Seq[Byte], downs: Seq[Script]): EDKleisli[Unit] = EDKleisli(_.addDowns(migHash, downs))

  def remove(hash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(_.remove(hash))

  def removeDowns(migHash: Seq[Byte]): EDKleisli[Unit] = EDKleisli(_.removeDowns(migHash))

  def applyScript(script: Script, direction: Direction): EDKleisli[Unit] = EDKleisli(_.applyScript(script, direction))

  def doTest(test: Test): EDKleisli[Unit] = EDKleisli(db => test.run(db))

  def close(): EDKleisli[Unit] = EDKleisli(_.close())
}
