package smt

import scalaz.\/

sealed trait DbAction[+R]

object DbAction {

  type SE[A] = (String \/ A)

  case object State extends DbAction[SE[Seq[MigrationInfo]]]

  case class Downs(hash: Seq[Byte]) extends DbAction[SE[Seq[Script]]]

  case class Add(migrationInfo: MigrationInfo) extends DbAction[SE[Unit]]

  case class AddDowns(migHash: Seq[Byte], downs: Seq[Script]) extends DbAction[SE[Unit]]

  case class Remove(hash: Seq[Byte]) extends DbAction[SE[Unit]]

  case class RemoveDowns(migHash: Seq[Byte]) extends DbAction[SE[Unit]]

  case class ApplyScript(script: Script, direction: Direction) extends DbAction[SE[Unit]]

  case class Failure(f: String) extends DbAction[SE[Nothing]]
}
