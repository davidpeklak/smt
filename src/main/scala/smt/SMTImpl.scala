package smt

import sbt._
import smt.db.Database
import sbt.Keys._
import report.Reporter
import java.io.File
import smt.migration.ScriptParsers._
import smt.migration.Migration
import smt.db.DbAction.HasDb
import smt.report.ReportersAction.HasReporters
import smt.describe.DescribeAction.HasLogger
import smt.db.AddAction.{HasUser, HasRemark}
import scalaz.\/

object SMTImpl {

  private def failException[T](s: TaskStreams)(e: String): T = {
    s.log.error(e)
    throw new Exception(e)
  }

  private def throwLeft[T](s: TaskStreams)(te: String \/ T): T = te.fold[T]((e: String) => failException(s)(e), identity)


  val stateHandling = new StateHandling[Database] {
    lazy val hasDb: HasDb[Database] = identity
  }

  val handling = new Handling[HandlingDep] {
    lazy val hasDb: HasDb[HandlingDep] = _.db
    lazy val hasLogger: HasLogger[HandlingDep] = _.logger
    lazy val hasReporters: HasReporters[HandlingDep] = _.rps
    lazy val hasUser: HasUser[HandlingDep] =  _.user
    lazy val hasRemark: HasRemark[HandlingDep] =  _.remark
  }

  def showDbState(db: Database, s: TaskStreams): Unit = {
    val result = stateHandling.state().run(db).run

    result.foreach(_.foreach(st => s.log.info(st.toString)))
    throwLeft(s)(result)
  }

  def showLatestCommon(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val result = stateHandling.latestCommon(ms zip MigrationHandling.hashMigrations(ms)).run(db).run

    result.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    throwLeft(s)(result)
  }

  def applyMigrations(args: Seq[String], db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, s: TaskStreams): Unit = {
    args match {
      case Seq(remark) => doApplyMigrations(db, ms, arb, runTests, rs, user, Some(remark), s)
      case Seq() => doApplyMigrations(db, ms, arb, runTests, rs, user, None, s)
      case _ => throw new Exception("Too many arguments. Optional remark expected.")
    }
  }

  def doApplyMigrations(db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, remark: Option[String], s: TaskStreams): Unit = {
    val action = handling.applyMigrationsAndReport(ms, arb, runTests)
    val dep = HandlingDep(db, rs.toList, s.log, user, remark)
    throwLeft(s)(action.run(dep).run)
  }

  def migrateTo(args: Seq[String], db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], user: String, s: TaskStreams) {
    def checkMig(target: String): Seq[Migration] = {
      val mst = ms.reverse.dropWhile(_.name != target).reverse
      if (mst.isEmpty) throw new Exception("No migration named '" + target + "' defined")
      else mst
    }

    args match {
      case Seq(target) => {
        val mst = checkMig(target)
        doApplyMigrations(db, mst, arb, runTests, rs, user, None, s)
      }
      case Seq(target, remark) => {
        val mst = checkMig(target)
        doApplyMigrations(db, mst, arb, runTests, rs, user, Some(remark), s)
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
        val script = OneFileOneScriptParser(fullPath).head
        val action = stateHandling.applyScript(script)
        action.run(db).run
      }
      case Seq() => throw new Exception("Path expected.")
      case _ => throw new Exception("Too many arguments. Path expected.")
    }
  }
}
