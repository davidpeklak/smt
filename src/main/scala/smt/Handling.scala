package smt

import smt.db._
import smt.report.{ReportersAction, Reporter}
import scalaz.Scalaz._
import smt.describe.DescribeAction
import sbt.Logger
import scalaz.{\/, -\/}
import smt.migration.{HashedMigrationSeq, MigrationInfo, Migration, Up}
import smt.util.EitherHaerte.EitherSyntax._

/**
  * uses functions of ConnectionHandling,
  * wraps around connection opening, locking and closing
  */
object StateHandling {

  def withConnection[U](action: (Connection, Logger) => String \/ U)(db: Database, logger: Logger): String \/ U = {
    for {
      c <- db.connection()
      r <- {
        c.init(logger) >>
          action(c, logger)
      }.andFinally(c.close(logger))
    } yield r
  }

  def withLock[U](action: (Connection, Logger) => String \/ U)(c: Connection, logger: Logger): String \/ U = {
    for {
      lock <- c.acquireLock(logger)
      r <- {
        action(c, logger)
      }.andFinally(c.releaseLock(logger)(lock))
    } yield r
  }

  def withLockedConnection[U](action: (Connection, Logger) => String \/ U)(db: Database, logger: Logger): String \/ U = {
    withConnection((c, l) => {
      withLock(action)(c, l)
    })(db, logger)
  }

  def state(db: Database, logger: Logger): String \/ Seq[MigrationInfo] = {
    withConnection((c, l) => c.state(l))(db, logger)
  }

  def latestCommon(mhs: HashedMigrationSeq)(db: Database, logger: Logger): String \/ Option[ConnectionHandling.Common] = {
    withConnection((c, l) => ConnectionHandling.latestCommon(mhs)(c, l))(db, logger)
  }

  def common(mhs: HashedMigrationSeq)(db: Database, logger: Logger): String \/ ConnectionHandling.CommonMigrations = {
    withConnection((c, l) => ConnectionHandling.common(mhs)(c, l))(db, logger)
  }

  def applyScript(scr: migration.Script)(db: Database, logger: Logger): String \/ Unit = {
    withLockedConnection((c, l) => c.applyScript(l)(scr, Up))(db, logger)
  }
}

/**
  * uses functions of AddHandling,
  * wraps around connection opening, locking and closing
  */
object Handling {

  def withConnection[U](action: (Connection, Logger, NamedMoveStatesHolder) => String \/ U)(db: Database, logger: Logger, nms: NamedMoveStatesHolder): String \/ U = {
    for {
      c <- db.connection()
      r <- {
        c.init(logger) >>
          action(c, logger, nms)
      }.andFinally(c.close(logger))
    } yield r
  }

  def withLock[U](action: (Connection, Logger, NamedMoveStatesHolder) => String \/ U)(c: Connection, logger: Logger, nms: NamedMoveStatesHolder): String \/ U = {
    for {
      lock <- c.acquireLock(logger)
      r <- {
        action(c, logger, nms)
      }.andFinally(c.releaseLock(logger)(lock))
    } yield r
  }

  def withLockedConnection[U](action: (Connection, Logger, NamedMoveStatesHolder) => String \/ U)(db: Database, logger: Logger, nms: NamedMoveStatesHolder): String \/ U = {
    withConnection((c, l, s) => {
      withLock(action)(c, l, s)
    })(db, logger, nms)
  }

  def applyMigrationsAndReport(ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, user: String, remark: String)(db: Database, logger: Logger, reporters: List[Reporter], nms: NamedMoveStatesHolder): String \/ Unit = {

    val e = withLockedConnection((c, l, s) => AddHandling.applyMigrations(ms, imo, arb, runTests, user, remark)(c, l, s))(db, logger, nms)

    (nms.nms.actions.lastOption, e) match {
      case (Some(ms), -\/(f)) => DescribeAction.describe(ms._1, ms._2, f)(logger)
      case (None, -\/(f)) => DescribeAction.describe(f)(logger)
      case _ => ()
    }

    ReportersAction.reportToAll(nms.nms)(reporters)

    e
  }
}
