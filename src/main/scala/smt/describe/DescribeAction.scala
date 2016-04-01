package smt.describe

import smt.{UpMoveState, DownMoveState, MoveState}
import sbt.Logger

object DescribeAction {
  def describe(f: String)(logger: Logger): Unit = {
    logger.warn(f)
  }


  def describe(migName: String, ms: MoveState, f: String)(logger: Logger): Unit = {
    ms match {
      case dms: DownMoveState => describeDms(migName, dms, f).foreach(l => logger.warn(l))
      case ums: UpMoveState => describeUms(migName, ums, f).foreach(l => logger.warn(l))
    }
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

    val upScriptsReport =
      if (appliedUps.isEmpty) Seq("No up scripts have been applied.")
      else Seq("Applied the following up scripts:") ++ appliedUps.map(_.toString)

    val crashedUpScriptReport =
      crashedUp.map(u => Seq("The following up script crashed:", u.toString)).getOrElse(Seq())

    val failure = Seq("Failure: " + f)

    val missingDowns =
      if (upsWithoutDowns.isEmpty) Seq()
      else Seq("The following up scripts have been applied without recording corresponding down scripts,",
        "revert them manually before continuing:") ++ upsWithoutDowns.map(_.name) ++
        Seq("", "************************************************************",
          "** DO NOT ATTEMPT REDELPOYMENT BEFORE THIS HAS BEEN FIXED **",
          "************************************************************")

    Seq("Failed to fully apply migration " + migName) ++
      upScriptsReport ++
      crashedUpScriptReport ++
      failure ++
      missingDowns
  }

}
