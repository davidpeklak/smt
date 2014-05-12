package smt

import smt.db.{Connection, ConnectionAction, DbAction, Database}
import smt.report.{ReporterAction, Reporter}
import scalaz._
import scalaz.concurrent.Future
import smt.migration.{MigrationInfo, Migration}
import scalaz.Scalaz._
import smt.util.ActionTypes
import smt.describe.DescribeAction
import sbt.Logger
import scalaz.-\/

case class HandlingDep(db: Database, rps: List[Reporter], log: Logger)

object Handling extends ActionTypes[HandlingDep] {

  val namedMoveTypes = writerTypes[NamedMoveStates]

  val dbActionTypes = new ActionTypes[Database] {}

  val dbActionNamedMoveTypes = dbActionTypes.writerTypes[NamedMoveStates]

  def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): dbActionTypes.EDKleisli[Option[DBHandling.Common]] = {
    import dbActionTypes.eSyntax._

    DbAction.connection() >=> {
      for {
        _ <- ConnectionAction.init()
        co <- DBHandling.latestCommon(mhs)
        _ <- ConnectionAction.close()
      } yield co
    }
  }

  def state(): dbActionTypes.EDKleisli[Seq[MigrationInfo]] = {
    import dbActionTypes.eSyntax._

    DbAction.connection() >=> {
      for {
        _ <- ConnectionAction.init()
        migs <- ConnectionAction.state()
        _ <- ConnectionAction.close()
      } yield migs
    }
  }

  def applyMigrationsAndReport(ms: Seq[Migration], arb: Boolean, runTests: Boolean): DKleisli[Unit] = {
    for {
      nmse <- {
        {
          import dbActionNamedMoveTypes._
          import dbActionNamedMoveTypes.ewSyntax._

          dbActionNamedMoveTypes.liftE(DbAction.connection()) >=> {
            import DBHandling.namedMoveTypes._
            liftE(ConnectionAction.init()) >>  DBHandling.applyMigrations(ms, arb, runTests) >> liftE(ConnectionAction.close())
          }
        }.run.run
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
