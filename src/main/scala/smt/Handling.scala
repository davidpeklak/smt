package smt

import smt.db.Database
import smt.report.{ReporterAction, Reporter}
import scalaz.Kleisli
import scalaz.concurrent.Future
import smt.migration.Migration
import scalaz.Scalaz._

object Handling {

  type RpsKleisli[+A] = Kleisli[Future, List[Reporter], A]

  def RpsKleisli[A](f: List[Reporter] => A): RpsKleisli[A] = Kleisli[Future, List[Reporter], A](rps => Future.delay(f(rps)))

  case class Dep(db: Database, rps: List[Reporter])

  type DepKleisli[A] = Kleisli[Future, Dep, A]

  def applyMigrationsAndReport(ms: Seq[Migration], arb: Boolean, runTests: Boolean): DepKleisli[Unit] = {
    for {
      nms <- {
        DBHandling.applyMigrations(ms, arb, runTests)
          .run.written
          .local[Dep](_.db)
      }

      _ <- {
        reportToAll(nms)
          .local[Dep](_.rps)

      }
    } yield ()
  }

  def reportToAll(nms: NamedMoveStates): RpsKleisli[Unit] = {
    Kleisli[Future, List[Reporter], Unit](_.traverse_(ReporterAction.report(nms).run))
  }
}
