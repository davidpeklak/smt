package smt

import java.util.Date
import util.Util._
import UpMoveState._
import DownMoveState._
import NamedMoveStates._
import scalaz.Scalaz._
import smt.db._
import smt.migration._
import smt.MigrationHandling._
import sbt.Logger
import smt.util.SeqHaerte.SeqSyntax._
import smt.util.EitherHaerte.EitherSyntax._

import scalaz.{\/-, -\/, \/}

case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

object ConnectionHandling {

  def now: Date = new Date

  case class Common(db: MigrationInfo, currentName: String) {
    override def toString: String = {
      val seq = Seq(Some(currentName + " (on db: " + db.name), Some(bytesToHex(db.hash)), Some(db.dateTime.toString), db.user, db.remark).flatten
      "CommonMigrationInfo(" + seq.mkString(", ") + ")"
    }
  }

  def latestCommon2(mis: Seq[MigrationInfo], ms: HashedMigrationSeq): Option[Common] = {
    common2(mis, ms).common.lastOption
  }

  case class CommonMigrations(
                               common: Seq[Common],
                               diffOnDb: Seq[MigrationInfo],
                               diffOnRepo: Seq[(Migration, Seq[Byte])]
                             )

  def common2(mis: Seq[MigrationInfo], ms: HashedMigrationSeq): CommonMigrations = {
    val misInit = mis.drop(ms.initMig)

    val (common, different) = (misInit zip ms.migs).span {
      case (MigrationInfo(_, hi, _, _, _), (_, h)) => hi == h
    }

    CommonMigrations(
      common = common.map {
        case (mi, (m, _)) => Common(mi, m.name)
      },
      diffOnDb = different.map(_._1),
      diffOnRepo = different.map(_._2)
    )
  }

  def latestCommon(mhs: HashedMigrationSeq)(connection: Connection, logger: Logger): String \/ Option[Common] = {
    connection.state(logger).map(latestCommon2(_, mhs))
  }

  def common(mhs: HashedMigrationSeq)(connection: Connection, logger: Logger): String \/ CommonMigrations = {
    connection.state(logger).map(common2(_, mhs))
  }

  def migrationsToRevert(latestCommon: Option[Seq[Byte]])(connection: Connection, logger: Logger): String \/ Seq[MigrationInfo] = {
    connection.state(logger).map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  def enrichMigrationWithDowns(mi: MigrationInfo)(connection: Connection, logger: Logger): String \/ MigrationInfoWithDowns = {
    connection.downs(logger)(mi.hash).map(MigrationInfoWithDowns(mi, _))
  }

  def testMigration(m: Migration)(connection: Connection, logger: Logger): String \/ Unit = {
    m.tests.travE_(_.run(connection)(logger))
  }

  def migrationsToApply(mhs: HashedMigrationSeq, latestCommon: Option[Seq[Byte]]): HashedMigrationSeq = {
    mhs.copy(migs = mhs.migs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse)
  }
}

object AddHandling {

  def rewriteMigration(mid: MigrationInfoWithDowns, dms: DownMoveState, hash: Seq[Byte], user: String, remark: String)(connection: Connection, logger: Logger): String \/ Unit = {
    val downsToWrite = mid.downs.reverse.map(Some(_)).zipAll(dms.appliedDowns.map(Some(_)) :+ dms.crashedDown, None, None)
      .dropWhile(t => t._1 == t._2).map(_._1.toSeq).flatten.reverse

    connection.addDowns(logger)(hash, downsToWrite) >>
      connection.add(logger, mid.mi.name, hash, ConnectionHandling.now, user, remark)
  }

  def revertMigration(mid: MigrationInfoWithDowns, user: String, remark: String)(connection: Connection, logger: Logger, dms: DownMoveStateHolder): String \/ Unit = {

    def applyScriptAndWrite(down: Script): String \/ Unit = {
      val r = connection.applyScript(logger)(down, Down)
      r match {
        case -\/(_) => dms.add(crashedDown(down))
        case \/-(()) => dms.add(appliedDown(down))
      }
      r
    }

    val r = connection.removeDowns(logger)(mid.mi.hash) >>
      connection.remove(logger)(mid.mi.hash) >>
      mid.downs.reverse.travE_(applyScriptAndWrite)

    r.onLeftDo(f => rewriteMigration(mid, dms.dms, failHash(f), user, remark)(connection, logger))
  }

  def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean, user: String, remark: String)(connection: Connection, logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    for {
      mis <- ConnectionHandling.migrationsToRevert(latestCommon)(connection, logger)
      mids <- mis.travE(mi => ConnectionHandling.enrichMigrationWithDowns(mi)(connection, logger))
      _ <- {
        if (mids.isEmpty || arb) mids.travE_(mid => nms.addDownMoveStateOf(mid.mi.name)(dms => revertMigration(mid, user, remark)(connection, logger, dms)))
        else -\/("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false")
      }
    } yield ()
  }

  def applyMigrations(ms: Seq[Migration], imo: Option[(Int, String)], arb: Boolean, runTests: Boolean, user: String, remark: String)(connection: Connection, logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    val mhs = hashMigrations(ms, imo)

    for {
      lcho <- ConnectionHandling.latestCommon(mhs)(connection, logger).map(_.map(_.db.hash))
      _ <- revertToLatestCommon(lcho, arb, user, remark)(connection, logger, nms)
      _ <- doApplyMigrations(mhs, lcho, runTests, user, remark)(connection, logger, nms)
    } yield ()
  }

  def doApplyMigrations(mhs: HashedMigrationSeq, latestCommon: Option[Seq[Byte]], runTests: Boolean, user: String, remark: String)(connection: Connection, logger: Logger, nms: NamedMoveStatesHolder): String \/ Unit = {
    ConnectionHandling.migrationsToApply(mhs, latestCommon).migs.travE_{
      case (m, h) => nms.addUpMoveStateOf(m.name)(ums => {
        if (runTests) for {
          _ <- applyMigration(m, h, user, remark)(connection, logger, ums)
          _ <- ConnectionHandling.testMigration(m)(connection, logger)
        } yield ()
        else applyMigration(m, h, user, remark)(connection, logger, ums)
      })
    }
  }

  def applyMigration(m: Migration, hash: Seq[Byte], user: String, remark: String)(connection: Connection, logger: Logger, ums: UpMoveStateHolder): String \/ Unit = {
    def finalize(downs: List[Script], hash: Seq[Byte]): String \/ Unit = {
      connection.addDowns(logger)(hash, downs) >>
        connection.add(logger, m.name, hash, ConnectionHandling.now, user, remark)
    }

    val r = m.groups.travE_(g => applyGroup(g)(connection, logger, ums))

    r match {
      case err@ -\/(f) => {
        finalize(ums.ums.downsToApply, failHash(f)) >>
          err
      }
      case \/-(()) => finalize(ums.ums.downsToApply, hash)
    }
  }

  def applyGroup(group: Group)(connection: Connection, logger: Logger, ums: UpMoveStateHolder): String \/ Unit = {
    def apl(up: Script): String \/ Unit = {
      val r = connection.applyScript(logger)(up, Up)
      r match {
        case -\/(_) => ums.add(crashedUp(up))
        case \/-(()) => ums.add(appliedUp(up))
      }
      r
    }

    val r = group.ups.travE_(apl)
    r match {
      case -\/(_) => {}
      case \/-(()) => {
        ums.add(downsToApply(group.downs.toList))
        ums.add(appliedUpsWithDowns(group.ups.toList))
      }
    }
    r
  }
}
