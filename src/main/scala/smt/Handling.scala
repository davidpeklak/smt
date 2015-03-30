package smt

import smt.db._
import smt.report.{ReportersAction, Reporter}
import scalaz.Scalaz._
import smt.describe.DescribeAction
import sbt.Logger
import scalaz.{-\/, EitherT}
import smt.migration.{HashedMigrationSeq, MigrationInfo, Migration, Up}
import smt.db.ConnectionAction.HasConnection
import smt.db.AddAction.{HasUser, HasRemark}
import smt.describe.DescribeAction.HasLogger

case class HandlingDep(db: Database, rps: List[Reporter], logger: Logger, user: String, remark: Option[String])

case class ConnectionDep(c: Connection, logger: Logger)

trait StateHandling[T] extends DbAction[T] {

  lazy val connectionHandling = new ConnectionHandling[ConnectionDep] {
    lazy val hasConnection: HasConnection[ConnectionDep] = _.c
    lazy val hasLogger: HasLogger[ConnectionDep] = _.logger
  }

  def openConnection: EDKleisli[ConnectionDep] = {
    for {
      c <- connection()
      l <- eAsk.map(hasLogger)
    } yield ConnectionDep(c, l)
  }

  def state(): EDKleisli[Seq[MigrationInfo]] = {
    import ekSyntax._
    import connectionHandling.eSyntax._

    openConnection >=> ((connectionHandling.init() >> connectionHandling.state()) andFinally connectionHandling.close())
  }

  def latestCommon(mhs: HashedMigrationSeq): EDKleisli[Option[connectionHandling.Common]] = {
    import ekSyntax._
    import connectionHandling.eSyntax._

    openConnection >=> ((connectionHandling.init() >> connectionHandling.latestCommon(mhs)) andFinally connectionHandling.close())
  }

  def common(mhs: HashedMigrationSeq): EDKleisli[connectionHandling.CommonMigrations] = {
    import ekSyntax._
    import connectionHandling.eSyntax._

    openConnection >=> ((connectionHandling.init() >> connectionHandling.common(mhs)) andFinally connectionHandling.close())
  }

  def applyScript(scr: migration.Script): EDKleisli[Unit] = {
    import ekSyntax._
    import connectionHandling.eSyntax._

    openConnection >=> (connectionHandling.applyScript(scr, Up) andFinally connectionHandling.close())
  }
}

trait Handling[T] extends DbAction[T] with DescribeAction[T] with ReportersAction[T] {

  handling =>

  val hasUser: HasUser[T]
  val hasRemark: HasRemark[T]

  lazy val connectionHandling = new AddHandling[(T, Connection)] {
    lazy val hasConnection: HasConnection[(T, Connection)] = _._2
    lazy val hasLogger: HasLogger[(T, Connection)] = t => handling.hasLogger(t._1)
    lazy val hasUser: HasUser[(T, Connection)] = t => handling.hasUser(t._1)
    lazy val hasRemark: HasRemark[(T, Connection)] = t => handling.hasRemark(t._1)
  }

  def applyMigrationsAndReport(ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean): EDKleisli[Unit] = {
    EitherT[DKleisli, String, Unit](
      for {
        nmse <- {
          {
            import namedMoveTypes._
            import namedMoveTypes.ewSyntax._

            {
              for {
                t <- namedMoveTypes.liftE(eAsk)
                c <- namedMoveTypes.liftE(connection())
              } yield {
                Tuple2(t, c)
              }
            } >=> {
              import connectionHandling.namedMoveTypes._
              liftE(connectionHandling.init()) >> connectionHandling.applyMigrations(ms, imo, arb, runTests) >> liftE(connectionHandling.close())
            }
          }.run.run
        }

        (nms, e) = nmse

        _ <- {
          (nms.actions.lastOption, e) match {
            case (Some(ms), -\/(f)) => describe(ms._1, ms._2, f)
            case (None, -\/(f)) => describe(f)
            case _ => point(())
          }
        }

        _ <- reportToAll(nms)
      } yield e
    )
  }
}
