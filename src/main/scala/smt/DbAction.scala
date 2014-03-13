package smt

sealed trait DbAction[+R]

object DbAction {

  case object State extends DbAction[Seq[MigrationInfo]]

  case class Downs(hash: Seq[Byte]) extends DbAction[Seq[Script]]

  case class Add(migrationInfo: MigrationInfo) extends DbAction[Unit]

  case class AddDowns(migHash: Seq[Byte], downs: Seq[Script]) extends DbAction[Unit]

  case class Remove(hash: Seq[Byte]) extends DbAction[Unit]

  case class RemoveDowns(migHash: Seq[Byte]) extends DbAction[Unit]

  case class ApplyScript(script: Script, direction: Direction) extends DbAction[Unit]

  case class TryApplyScript(script: Script, direction: Direction) extends DbAction[Option[String]] // Some[String] represents a failure

  case class Failure(f: String) extends DbAction[Nothing]
}
