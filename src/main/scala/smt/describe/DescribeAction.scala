package smt.describe

import smt.util.ActionTypes
import smt.{UpMoveState, DownMoveState, MoveState}
import sbt.Logger

object HasLogger {
  type HasLogger[α] = α => Logger
}

trait DescribeAction[T] extends ActionTypes[T] {

  val hasLogger: HasLogger.HasLogger[T]

  def describe(f: String): DKleisli[Unit] = {
    DKleisli(t => hasLogger(t).warn(f))
  }

  def describe(migName: String, ms: MoveState, f: String): DKleisli[Unit] = {
    DKleisli(t => ms match {
      case dms: DownMoveState => describeDms(migName, dms, f).foreach(l => hasLogger(t).warn(l))
      case ums: UpMoveState => describeUms(migName, ums, f).foreach(l => hasLogger(t).warn(l))
    })
  }

  private def describeDms(migName: String, dms: DownMoveState, f: String): Seq[String] = {
    import dms._
    Seq("Failed to fully revert migration " + migName, "Applied the following down scripts:") ++
      appliedDowns.map(_.toString) ++
      Seq("The following down script crashed:") ++
      Seq(crashedDown.map(_.toString).getOrElse("(None)")) ++
      Seq("Because of: " + f.toString, "Apply the intended effect of the crashed script manually before continuing.")
  }

  private def describeUms(migName: String, ums: UpMoveState, f: String): Seq[String] = {
    import ums._
    val upsWithoutDowns = appliedUpsWithDowns.map(Some(_)).zipAll(appliedUps.map(Some(_)), None, None)
      .dropWhile(t => t._1 == t._2).map(_._2.toSeq).flatten

    Seq("Failed to fully apply migration " + migName, "Applied the following up scripts:") ++
      appliedUps.map(_.toString) ++
      Seq("The following up script crashed:") ++
      Seq(crashedUp.map(_.toString).getOrElse("(None)")) ++
      Seq("Because of: " + f.toString, "The following up scripts have been applied without recording corresponding down scripts,", "revert them manually before continuing:") ++
      upsWithoutDowns.map(_.name)
  }

}
