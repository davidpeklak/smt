package smt

import smt.db._
import smt.report.{ReporterAction, Reporter}
import scalaz._
import scalaz.concurrent.Future
import smt.migration.{MigrationInfo, Migration}
import scalaz.Scalaz._
import smt.util.ActionTypes
import smt.describe.DescribeAction
import sbt.Logger
import scalaz.-\/
import scalaz.-\/
import smt.migration.MigrationInfo
import smt.migration.Migration
import scala.Some


trait HasLogger {
  val logger: Logger
}

case class HasLogOnly(logger: Logger)


case class HandlingDep(db: Database, rps: List[Reporter], logger: Logger) extends HasDb

trait StateHandling[T <: HasDb] extends ActionTypes[T] {

  val dbAction = new DbAction[T] { }

  val connectionAction = new ConnectionAction[HasConnectionOnly] { }

  def state(): EDKleisli[Seq[MigrationInfo]] = {
    import eSyntax._

    dbAction.connection().map(HasConnectionOnly) >=> {
      for {
        _ <- connectionAction.init()
        migs <- connectionAction.state()
        _ <- connectionAction.close()
      } yield migs
    }
  }

  def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): EDKleisli[Option[DBHandling.Common]] = {
    import eSyntax._

    dbAction.connection().map(HasConnectionOnly) >=> {
      for {
        _ <- connectionAction.init()
        co <- DBHandling.latestCommon(mhs)
        _ <- connectionAction.close()
      } yield co
    }
  }
}

object Handling extends ActionTypes[HandlingDep] {

  val dbAction = new DbAction[HandlingDep] {}

  val connectionAction = new ConnectionAction[HasConnectionOnly] { }

  def applyMigrationsAndReport(ms: Seq[Migration], arb: Boolean, runTests: Boolean): DKleisli[Unit] = {
    for {
      nmse <- {
        {
          import dbAction.namedMoveTypes._
          import dbAction.namedMoveTypes.ewSyntax._

          dbAction.namedMoveTypes.liftE(dbAction.connection().map(HasConnectionOnly)) >=> {
            import connectionAction.namedMoveTypes._
            liftE(connectionAction.init()) >>  DBHandling.applyMigrations(ms, arb, runTests) >> liftE(connectionAction.close())
          }
        }.run.run
      }

      (nms, e) = nmse

      _ <- {
        (nms.actions.lastOption, e) match {
          case (Some(ms), -\/(f)) => {
            DescribeAction.describe(ms._1, ms._2, f)
              .local[HandlingDep](_.logger)
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
