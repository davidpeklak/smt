package smt

import sbt._
import smt.db.Database
import sbt.Keys._
import report.Reporter
import java.io.File
import smt.migration.FileSplitters._
import smt.migration.Migration
import scalaz.\/

object SMTImpl {

  private def failException[T](s: TaskStreams)(e: String): T = {
    s.log.error(e)
    throw new Exception(e)
  }

  private def throwLeft[T](s: TaskStreams)(te: String \/ T): T = te.fold[T]((e: String) => failException(s)(e), identity)

  def showDbState(db: Database, ms: Seq[Migration], imo: Option[(Int, String)], s: TaskStreams): Unit = {

    val result = StateHandling.common(MigrationHandling.hashMigrations(ms, imo))(db, s.log)

    result.foreach(co => {
      co.common.foreach(st => s.log.info(st.toString))
      co.diffOnDb.foreach(st => s.log.info("(!) " + st.toString))
    })
    throwLeft(s)(result)
  }

  def showLatestCommon(db: Database, ms: Seq[Migration], imo: Option[(Int, String)], s: TaskStreams): Unit = {
    val result = StateHandling.latestCommon(MigrationHandling.hashMigrations(ms, imo))(db, s.log)

    result.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    throwLeft(s)(result)
  }

  def applyMigrations(args: Seq[String], db: Database, ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, s: TaskStreams): Unit = {
    args match {
      case Seq(remark) => doApplyMigrations(db, ms, imo, arb, runTests, rs, user, Some(remark), s)
      case Seq() => doApplyMigrations(db, ms, imo, arb, runTests, rs, user, None, s)
      case _ => throw new Exception("Too many arguments. Optional remark expected.")
    }
  }

  def doApplyMigrations(db: Database, ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, remark: Option[String], s: TaskStreams): Unit = {
    val result = Handling.applyMigrationsAndReport(ms, imo, arb, runTests, user, remark.getOrElse(""))(db, s.log, rs.toList, new NamedMoveStatesHolder())
    throwLeft(s)(result)
  }

  def migrateTo(args: Seq[String], db: Database, ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, s: TaskStreams) {
    def checkMig(target: String): Seq[Migration] = {
      val mst = ms.reverse.dropWhile(_.name != target).reverse
      if (mst.isEmpty) throw new Exception("No migration named '" + target + "' defined")
      else mst
    }

    args match {
      case Seq(target) => {
        val mst = checkMig(target)
        doApplyMigrations(db, mst, imo, arb, runTests, rs, user, None, s)
      }
      case Seq(target, remark) => {
        val mst = checkMig(target)
        doApplyMigrations(db, mst, imo, arb, runTests, rs, user, Some(remark), s)
      }
      case Seq() => throw new Exception("Name of a migration expected.")
      case _ => throw new Exception("Too many arguments. Name of a migration and optional remark expected.")
    }
  }

  def runScript(args: Seq[String], sourceDir: File, db: Database, s: TaskStreams) {
    args match {
      case Seq(dir) => {
        val relPath = IO.pathSplit(dir).toSeq
        val fullPath = relPath.foldLeft[File](sourceDir)((p, s) => p / s)
        val script = OneFileOneScriptSplitter(fullPath).head
        val result = StateHandling.applyScript(script)(db, s.log)
        throwLeft(s)(result)
      }
      case Seq() => throw new Exception("Path expected.")
      case _ => throw new Exception("Too many arguments. Path expected.")
    }
  }
}
