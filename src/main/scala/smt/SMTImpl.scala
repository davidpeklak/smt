package smt

import smt.db.{Database, DbAction}
import sbt.Keys._
import smt.migration.Migration
import report.Reporter

object SMTImpl {

  private def failException(s: TaskStreams)(e: String) {
    s.log.error(e)
    throw new Exception(e)
  }

  def showDbState(db: Database, s: TaskStreams): Unit = {
    val result = DbAction.state.run(db).run

    result.foreach(_.foreach(st => s.log.info(st.toString)))
    result.swap.foreach(failException(s))
  }

  def showLatestCommon(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val result = DBHandling.latestCommon(ms zip MigrationHandling.hashMigrations(ms)).run(db).run

    result.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    result.swap.foreach(failException(s))
  }

  def applyMigrations(db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], s: TaskStreams): Unit = {
    val action = Handling.applyMigrationsAndReport(ms, arb, runTests)
    val dep = HandlingDep(db, rs.toList, s.log)
    action.run(dep).run
  }

  def migrateTo(args: Seq[String], db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, rs: Seq[Reporter], s: TaskStreams) {
    args match {
      case Seq(target) => {
        val mst = ms.reverse.dropWhile(_.name != target).reverse
        if (mst.isEmpty) throw new Exception("No migration named '" + target + "' defined")
        else {
          applyMigrations(db, mst, arb, runTests, rs, s)
        }
      }
      case Seq() => throw new Exception("Name of a migration expected.")
      case _ => throw new Exception("Too many arguments. Name of a migration expected.")
    }
  }
}
