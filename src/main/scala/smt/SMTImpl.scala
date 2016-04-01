package smt

import sbt._
import smt.db.{DatabaseId, MetaDatabase, Database}
import sbt.Keys._
import report.Reporter
import java.io.File
import smt.migration.FileSplitters._
import smt.migration.Script
import smt.migration.Migration
import scalaz.\/

object SMTImpl {

  private def failException[T](s: TaskStreams)(e: String): T = {
    s.log.error(e)
    throw new Exception(e)
  }

  private def throwLeft[T](s: TaskStreams)(te: String \/ T): T = te.fold[T]((e: String) => failException(s)(e), identity)

  def showDbState(metaDb: MetaDatabase, ms: Seq[Migration], imo: Option[(Int, String)], s: TaskStreams): Unit = {

    val result = StateHandling.common(MigrationHandling.hashMigrations(ms, imo))(metaDb, s.log)

    result.foreach(co => {
      co.common.foreach(st => s.log.info(st.toString))
      co.diffOnDb.foreach(st => s.log.info("(!) " + st.toString))
    })
    throwLeft(s)(result)
  }

  def showLatestCommon(metaDb: MetaDatabase, ms: Seq[Migration], imo: Option[(Int, String)], s: TaskStreams): Unit = {
    val result = StateHandling.latestCommon(MigrationHandling.hashMigrations(ms, imo))(metaDb, s.log)

    result.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    throwLeft(s)(result)
  }

  def applyMigrations(args: Seq[String], metaDb: MetaDatabase, databases: Map[DatabaseId, Database], ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, s: TaskStreams): Unit = {
    val remarkOpt = args match {
      case Seq(remark) => Some(remark)
      case Seq() => None
      case _ => throw new Exception("Too many arguments. Optional remark expected.")
    }
    doApplyMigrations(metaDb, databases, ms, imo, arb, runTests, rs, user, remarkOpt, s)
  }

  def doApplyMigrations(metaDb: MetaDatabase, databases: Map[DatabaseId, Database], ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, remark: Option[String], s: TaskStreams): Unit = {
    val result = Handling.applyMigrationsAndReport(ms, imo, arb, runTests, user, remark.getOrElse(""))(metaDb, databases, s.log, rs.toList)
    throwLeft(s)(result)
  }

  def migrateTo(args: Seq[String], metaDb: MetaDatabase, databases: Map[DatabaseId, Database], ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, s: TaskStreams) {
    def checkMig(target: String): Seq[Migration] = {
      val mst = ms.reverse.dropWhile(_.name != target).reverse
      if (mst.isEmpty) throw new Exception("No migration named '" + target + "' defined")
      else mst
    }

    args match {
      case Seq(target) => {
        val mst = checkMig(target)
        doApplyMigrations(metaDb, databases, mst, imo, arb, runTests, rs, user, None, s)
      }
      case Seq(target, remark) => {
        val mst = checkMig(target)
        doApplyMigrations(metaDb, databases, mst, imo, arb, runTests, rs, user, Some(remark), s)
      }
      case Seq() => throw new Exception("Name of a migration expected.")
      case _ => throw new Exception("Too many arguments. Name of a migration and optional remark expected.")
    }
  }

  def runScript(args: Seq[String], sourceDir: File, metaDb: MetaDatabase, databases: Map[DatabaseId, Database], s: TaskStreams) {

    def getScript(dir: String): Script = {
      val relPath = IO.pathSplit(dir).toSeq
      val fullPath = relPath.foldLeft[File](sourceDir)((p, s) => p / s)
      OneFileOneScriptSplitter(fullPath).head
    }

    args match {
      case Seq(dir, databaseId) => {
        val script = getScript(dir)
        val dbId = DatabaseId(databaseId)
        val result = Handling.applyScript(script, dbId)(metaDb, databases, s.log)
        throwLeft(s)(result)
      }
      case Seq(dir) => throw new Exception("Missing argument. Path and DatabaseId expected.")
      case _ => throw new Exception("Too many arguments. Path and DatabaseId expected.")
    }
  }
}
