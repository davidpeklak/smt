package smt

import smt.db._
import smt.report.{ReportersAction, Reporter}
import scalaz.Scalaz._
import smt.describe.DescribeAction
import smt.util.Logger
import scalaz.{\/, -\/}
import smt.migration.{HashedMigrationSeq, MigrationInfo, Migration, Up}
import smt.util.EitherSyntax._

/**
  * uses functions of MetaConnectionHandling,
  * wraps around connection opening, locking and closing
  */
object StateHandling {

  def withMetaConnection[U](effect: (MetaConnection, Logger) => String \/ U)(db: MetaDatabase, logger: Logger): String \/ U = {
    for {
      c <- db.connection()
      r <- {
        c.init(logger) >>
          effect(c, logger)
      }.andFinally(c.close(logger))
    } yield r
  }

  def withLock[U](effect: (MetaConnection, Logger) => String \/ U)(c: MetaConnection, logger: Logger): String \/ U = {
    for {
      lock <- c.acquireLock(logger)
      r <- {
        effect(c, logger)
      }.andFinally(c.releaseLock(logger)(lock))
    } yield r
  }

  def withLockedMetaConnection[U](effect: (MetaConnection, Logger) => String \/ U)(db: MetaDatabase, logger: Logger): String \/ U = {
    withMetaConnection((c, l) => {
      withLock(effect)(c, l)
    })(db, logger)
  }

  def state(db: MetaDatabase, logger: Logger): String \/ Seq[MigrationInfo] = {
    withMetaConnection((c, l) => c.state(l))(db, logger)
  }

  def latestCommon(mhs: HashedMigrationSeq)(db: MetaDatabase, logger: Logger): String \/ Option[MigrationHandling.Common] = {
    withMetaConnection((c, l) => MetaConnectionHandling.latestCommon(mhs)(c, l))(db, logger)
  }

  def common(mhs: HashedMigrationSeq)(db: MetaDatabase, logger: Logger): String \/ MigrationHandling.CommonMigrations = {
    withMetaConnection((c, l) => MetaConnectionHandling.common(mhs)(c, l))(db, logger)
  }
}

/**
  * uses functions of MutatingHandling,
  * wraps around connection opening, locking and closing
  */
object Handling {

  def withMetaConnection[U](effect: (MetaConnection, Logger, NamedMoveStatesHolder) => String \/ U)(metaDb: MetaDatabase, logger: Logger, nms: NamedMoveStatesHolder): String \/ U = {
    for {
      c <- metaDb.connection()
      r <- {
        c.init(logger) >>
          effect(c, logger, nms)
      }.andFinally(c.close(logger))
    } yield r
  }

  def withLock[U](effect: (MetaConnection, Logger, NamedMoveStatesHolder) => String \/ U)(c: MetaConnection, logger: Logger, nms: NamedMoveStatesHolder): String \/ U = {
    for {
      lock <- c.acquireLock(logger)
      r <- {
        effect(c, logger, nms)
      }.andFinally(c.releaseLock(logger)(lock))
    } yield r
  }

  def withLockedMetaConnection[U](effect: (MetaConnection, Logger, NamedMoveStatesHolder) => String \/ U)(metaDb: MetaDatabase, logger: Logger, nms: NamedMoveStatesHolder): String \/ U = {
    withMetaConnection((c, l, s) => {
      withLock(effect)(c, l, s)
    })(metaDb, logger, nms)
  }

  def applyScript(scr: migration.Script, dbId: DatabaseId)(metaDb: MetaDatabase, databases: Map[DatabaseId, Database], logger: Logger): String \/ Unit = {
    withLockedMetaConnection((c, l, nms) =>
      MulipleDatabasesHandling.withConnection(dbId, _.applyScript(l)(scr, Up))(databases, l))(metaDb, logger, new NamedMoveStatesHolder)
  }

  def applyMigrationsAndReport(ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, user: String, remark: String)(metaDb: MetaDatabase, databases: Map[DatabaseId, Database], logger: Logger, reporters: List[Reporter]): String \/ Unit = {
    val nms = new NamedMoveStatesHolder

    val e = withLockedMetaConnection((c, l, nms) =>
      MulipleDatabasesHandling.applyMigrations(ms, imo, arb, runTests, user, remark)(c, databases, l, nms))(metaDb, logger, nms)

    (nms.nms.actions.lastOption, e) match {
      case (Some(ms), -\/(f)) => DescribeAction.describe(ms._1, ms._2, f)(logger)
      case (None, -\/(f)) => DescribeAction.describe(f)(logger)
      case _ => ()
    }

    ReportersAction.reportToAll(nms.nms)(reporters)

    e
  }
}
