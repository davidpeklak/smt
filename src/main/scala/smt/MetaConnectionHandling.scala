package smt

import java.util.Date
import smt.db._
import smt.migration._
import smt.MigrationHandling._
import sbt.Logger
import smt.util.SeqHaerte.SeqSyntax._
import smt.util.EitherHaerte.EitherSyntax._
import scalaz.\/
import scalaz.Scalaz._

object MetaConnectionHandling {

  case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  def now(): Date = new Date

  def latestCommon(mhs: HashedMigrationSeq)(metaConnection: MetaConnection, logger: Logger): String \/ Option[Common] = {
    metaConnection.state(logger).map(latestCommon2(_, mhs))
  }

  def common(mhs: HashedMigrationSeq)(metaConnection: MetaConnection, logger: Logger): String \/ CommonMigrations = {
    metaConnection.state(logger).map(common2(_, mhs))
  }

  def migrationsToRevert(latestCommon: Option[Seq[Byte]])(metaConnection: MetaConnection, logger: Logger): String \/ Seq[MigrationInfo] = {
    metaConnection.state(logger).map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  def enrichMigrationWithDowns(mi: MigrationInfo)(metaConnection: MetaConnection, logger: Logger): String \/ MigrationInfoWithDowns = {
    metaConnection.downs(logger)(mi.hash).map(MigrationInfoWithDowns(mi, _))
  }

  def rewriteMigration(mid: MigrationInfoWithDowns, dms: DownMoveState, hash: Seq[Byte], user: String, remark: String)(metaConnection: MetaConnection, logger: Logger): String \/ Unit = {
    val downsToWrite = mid.downs.reverse.map(Some(_)).zipAll(dms.appliedDowns.map(Some(_)) :+ dms.crashedDown, None, None)
      .dropWhile(t => t._1 == t._2).map(_._1.toSeq).flatten.reverse

    metaConnection.addDowns(logger)(hash, downsToWrite) >>
      metaConnection.add(logger, mid.mi.name, mid.mi.dbId, hash, now(), user, remark)
  }
}
