package smt

import smt.db._
import smt.report.{ReportersAction, Reporter}
import scalaz.Scalaz._
import smt.describe.DescribeAction
import sbt.Logger
import scalaz.{-\/, EitherT}
import smt.migration.{HashedMigrationSeq, MigrationInfo, Migration, Up}
import smt.db.ConnectionAction.HasConnection
import smt.db.LockAction.HasLock
import smt.db.AddAction.{HasUser, HasRemark}
import smt.describe.DescribeAction.HasLogger

case class HandlingDep(db: Database, rps: List[Reporter], logger: Logger, user: String, remark: Option[String])

case class ConnectionDep(c: Connection, logger: Logger)

trait StateHandling[T] extends DbAction[T] {

  lazy val connectionHandling = new ConnectionHandling[ConnectionDep] {
    lazy val hasConnection: HasConnection[ConnectionDep] = _.c
    lazy val hasLogger: HasLogger[ConnectionDep] = _.logger
  }

  lazy val connectionLockHandling = new ConnectionHandling[(ConnectionDep, String)] {
    lazy val hasConnection: HasConnection[(ConnectionDep, String)] = _._1.c
    lazy val hasLogger: HasLogger[(ConnectionDep, String)] = _._1.logger
    lazy val hasLock: HasLock[(ConnectionDep, String)] = _._2
  }

  def openConnection: EDKleisli[ConnectionDep] = {
    for {
      c <- connection()
      l <- eAsk.map(hasLogger)
    } yield ConnectionDep(c, l)
  }

  def withConnection[U](action: connectionHandling.EDKleisli[U]): EDKleisli[U] = {
    import ekSyntax._
    import connectionHandling.eSyntax._

    openConnection >=> ((connectionHandling.init() >> action) andFinally connectionHandling.close())
  }

  def acqLock: connectionHandling.EDKleisli[(ConnectionDep, String)] = {
    for {
      t <- connectionHandling.eAsk
      l <- connectionHandling.acquireLock()
    } yield (t, l)
  }

  def rlsLock: connectionLockHandling.EDKleisli[Unit] = {
    for {
      t <- connectionLockHandling.eAsk
      _ <- connectionLockHandling.releaseLock(t._2)
    } yield ()
  }

  def withLock[U](action: connectionLockHandling.EDKleisli[U]): connectionHandling.EDKleisli[U] = {
    import connectionHandling._
    import connectionHandling.ekSyntax._

    acqLock >=> {
      import connectionLockHandling._
      import connectionLockHandling.eSyntax._
      action andFinally rlsLock
    }
  }

  def state(): EDKleisli[Seq[MigrationInfo]] = withConnection(connectionHandling.state())

  def latestCommon(mhs: HashedMigrationSeq): EDKleisli[Option[connectionHandling.Common]] = withConnection(connectionHandling.latestCommon(mhs))

  def common(mhs: HashedMigrationSeq): EDKleisli[connectionHandling.CommonMigrations] = withConnection(connectionHandling.common(mhs))

  def applyScript(scr: migration.Script): EDKleisli[Unit] = {
    import ekSyntax._
    import connectionHandling.eSyntax._

    openConnection >=> (withLock(connectionLockHandling.applyScript(scr, Up)) andFinally connectionHandling.close())
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

  lazy val connectionLockHandling = new AddHandling[(T, Connection, String)] {
    lazy val hasConnection: HasConnection[(T, Connection, String)] = _._2
    lazy val hasLogger: HasLogger[(T, Connection, String)] = t => handling.hasLogger(t._1)
    lazy val hasUser: HasUser[(T, Connection, String)] = t => handling.hasUser(t._1)
    lazy val hasRemark: HasRemark[(T, Connection, String)] = t => handling.hasRemark(t._1)
    lazy val hasLock: HasLock[(T, Connection, String)] = _._3
  }

  def applyMigrationsAndReport(ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean): EDKleisli[Unit] = {

    def openConnection: EDKleisli[(T, Connection)] = {
      for {
        t <- eAsk
        c <- connection()
      } yield Tuple2(t, c)
    }

    def withConnection[U](action: connectionHandling.namedMoveTypes.EWDKleisli[U]): namedMoveTypes.EWDKleisli[U] = {
      import namedMoveTypes._
      import namedMoveTypes.ewSyntax._

      namedMoveTypes.liftE(openConnection) >=> {
        import connectionHandling.namedMoveTypes._
        import connectionHandling.namedMoveTypes.ewSyntax2._
        (liftE(connectionHandling.init()) >> action) andFinally liftE(connectionHandling.close())
      }
    }

    def acqLock: connectionHandling.EDKleisli[(T, Connection, String)] = {
      for {
        t <- connectionHandling.eAsk
        l <- connectionHandling.acquireLock()
      } yield (t._1, t._2, l)
    }

    def rlsLock: connectionLockHandling.EDKleisli[Unit] = {
      for {
        t <- connectionLockHandling.eAsk
        _ <- connectionLockHandling.releaseLock(t._3)
      } yield ()
    }

    def withLock[U](action: connectionLockHandling.namedMoveTypes.EWDKleisli[U]): connectionHandling.namedMoveTypes.EWDKleisli[U] = {
      import connectionHandling.namedMoveTypes._
      import connectionHandling.namedMoveTypes.ewSyntax._

      connectionHandling.namedMoveTypes.liftE(acqLock) >=> {
        import connectionLockHandling.namedMoveTypes._
        import connectionLockHandling.namedMoveTypes.ewSyntax2._
        action andFinally liftE(rlsLock)
      }
    }

    EitherT[DKleisli, String, Unit](
      for {
        nmse <- withConnection(withLock(connectionLockHandling.applyMigrations(ms, imo, arb, runTests))).run.run

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
