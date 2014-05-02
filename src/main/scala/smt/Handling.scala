package smt

import smt.db.Database
import smt.report.{ReporterAction, Reporter}
import scalaz.{Monad, \/, -\/, Kleisli}
import scalaz.concurrent.Future
import smt.migration.Migration
import scalaz.Scalaz._
import smt.util.ActionTypes
import smt.describe.DescribeAction
import sbt.Logger

case class HandlingDep(db: Database, rps: List[Reporter], log: Logger)

object Handling extends ActionTypes[HandlingDep] {

  def applyMigrationsAndReport(ms: Seq[Migration], arb: Boolean, runTests: Boolean): DKleisli[Unit] = {
    for {
      nmse <- {
        DBHandling.applyMigrations(ms, arb, runTests)
          .run.run
          .local[HandlingDep](_.db)
      }

      (nms, e) = nmse

      _ <- {
        (nms.actions.lastOption, e) match {
          case (Some(ms), -\/(f)) => {
            DescribeAction.describe(ms._1, ms._2, f)
              .local[HandlingDep](_.log)
          }
          case _ => point(())
        }
      }

      _ <- {
        reportToAll(nms)
          .local[HandlingDep](_.rps)

      }
    } yield ()
  }

  def reportToAll(nms: NamedMoveStates): Kleisli[Future, List[Reporter], Unit] = {
    Kleisli[Future, List[Reporter], Unit](_.traverse_(ReporterAction.report(nms).run))
  }
}
