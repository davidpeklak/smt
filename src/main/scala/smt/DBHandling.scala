package smt

import java.util.Date
import util.Util._
import UpMoveState._
import DownMoveState._
import NamedMoveStates._
import scalaz.Scalaz._
import smt.util.TraverseStackSafeSyntax
import TraverseStackSafeSyntax._
import smt.db._
import smt.migration._
import smt.migration.Group
import smt.migration.MigrationInfo
import scala.Some
import smt.migration.Script
import smt.migration.Migration

object DBHandling extends ConnectionAction[HasConnectionOnly] {

  import MigrationHandling._

  def now: Date = new Date

  case class Common(db: MigrationInfo, currentName: String) {
    override def toString: String = {
      "CommonMigrationInfo(" + currentName + " (on db: " + db.name + "), " + bytesToHex(db.hash) + ", " + db.dateTime + ")"
    }
  }

  private def describe(migName: String, dms: DownMoveState, f: String): String = {
    import dms._
    "Failed to fully revert migration " + migName + "\n" +
      "Applied the following down scripts: \n" +
      dms.appliedDowns.mkString("\n") + "\n" +
      "The following down script crashed: \n" +
      dms.crashedDown.getOrElse("(None)") + "\n" +
      "Because of: " + f.toString + "\n" +
      "Apply the intended effect of the crashed script manually before continuing."
  }

  private def describe(migName: String, ums: UpMoveState, f: String): String = {
    import ums._
    val upsWithoutDowns = appliedUpsWithDowns.map(Some(_)).zipAll(appliedUps.map(Some(_)), None, None)
      .dropWhile(t => t._1 == t._2).map(_._2.toSeq).flatten

    "Failed to fully apply migration " + migName + "\n" +
      "Applied the following up scripts: \n" +
      ums.appliedUps.mkString("\n") + "\n" +
      "The following up script crashed: \n" +
      ums.crashedUp.getOrElse("(None)") + "\n" +
      "Because of: " + f.toString + "\n" +
      "The following up scripts have been applied without recording corresponding down scripts,\n" +
      "revert them manually before continuing:" + "\n" +
      upsWithoutDowns.map(_.name).mkString("\n")
  }

  private def latestCommon2(mis: Seq[MigrationInfo], ms: Seq[(Migration, Seq[Byte])]): Option[Common] = {
    (mis zip ms).takeWhile {
      case (MigrationInfo(_, hi, _), (_, h)) => hi == h
    }.lastOption.map {
      case (mi, (m, _)) => Common(mi, m.name)
    }
  }

  def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): EDKleisli[Option[Common]] = {
    state().map(latestCommon2(_, mhs))
  }

  def applyMigrations(ms: Seq[Migration], arb: Boolean, runTests: Boolean): namedMoveTypes.EWDKleisli[Unit] = {
    import namedMoveTypes._

    val mhs = ms zip hashMigrations(ms)

    for {
      lcho <- liftE(latestCommon(mhs).map(_.map(_.db.hash)))
      _ <- revertToLatestCommon(lcho, arb)
      _ <- applyMigrations(mhs, lcho, runTests)
    } yield ()
  }

  case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean): namedMoveTypes.EWDKleisli[Unit] = {
    import namedMoveTypes._
    import downMoveTypes.EWSyntax._

    for {
      mis <- liftE(migrationsToRevert(latestCommon))
      mids <- liftE(mis.toList.traverse(enrichMigrationWithDowns))
      _ <- {
        if (mids.isEmpty || arb) mids.toList.traverse[EWDKleisli, Unit](mid => revertMigration(mid).mapWritten(namedMoveState(mid.mi.name)))
        else liftE(failure("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false"))
      }
    } yield ()

  }

  private def migrationsToRevert(latestCommon: Option[Seq[Byte]]): EDKleisli[Seq[MigrationInfo]] = {
    state().map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  private def enrichMigrationWithDowns(mi: MigrationInfo): EDKleisli[MigrationInfoWithDowns] = {
    downs(mi.hash).map(MigrationInfoWithDowns(mi, _))
  }

  def rewriteMigration(mid: MigrationInfoWithDowns, dms: DownMoveState, hash: Seq[Byte]): EDKleisli[Unit] = {
    val downsToWrite = mid.downs.reverse.map(Some(_)).zipAll(dms.appliedDowns.map(Some(_)) :+ dms.crashedDown, None, None)
      .dropWhile(t => t._1 == t._2).map(_._1.toSeq).flatten.reverse

    addDowns(hash, downsToWrite) >> add(MigrationInfo(name = mid.mi.name, hash = hash, dateTime = now))
  }

  def revertMigration(mid: MigrationInfoWithDowns): downMoveTypes.EWDKleisli[Unit] = {
    import downMoveTypes._
    import EWSyntax._

    def applyScriptAndWrite(down: Script): EWDKleisli[Unit] = {
      liftE(applyScript(down, Down)) :-\/++> crashedDown(down) :\/-++> appliedDown(down)
    }

    liftE(removeDowns(mid.mi.hash) >> remove(mid.mi.hash)) >>
      mid.downs.reverse.toList.traverse__(applyScriptAndWrite).recover((dms, f) => {
        liftE(rewriteMigration(mid, dms, failHash(f)) >> failure(describe(mid.mi.name, dms, f)))
      })
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]], runTests: Boolean): namedMoveTypes.EWDKleisli[Unit] = {
    import namedMoveTypes._
    import upMoveTypes.EWSyntax._

    migrationsToApply(mhs, latestCommon).toList.traverse__ {
      case (m, h) =>
        if (runTests) for {
          _ <- applyMigration(m, h).mapWritten(namedMoveState(m.name))
          _ <- liftE(testMigration(m))
        } yield ()
        else applyMigration(m, h).mapWritten(namedMoveState(m.name))
    }
  }

  private def testMigration(m: Migration): EDKleisli[Unit] = {
    m.tests.toList.traverse__(doTest)
  }

  private def migrationsToApply(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[(Migration, Seq[Byte])] = {
    mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
  }

  private def applyGroup(group: Group): upMoveTypes.EWDKleisli[Unit] = {
    import upMoveTypes._
    import EWSyntax._

    def apl(up: Script): upMoveTypes.EWDKleisli[Unit] = {
      liftE(applyScript(up, Up)) :-\/++> crashedUp(up) :\/-++> appliedUp(up)
    }

    group.ups.toList.traverse__(apl) :\/-++> (downsToApply(group.downs.toList) ⊹ appliedUpsWithDowns(group.ups.toList))
  }

  def applyMigration(m: Migration, hash: Seq[Byte]): upMoveTypes.EWDKleisli[Unit] = {
    import upMoveTypes._
    import EWSyntax._

    def finalize(downs: List[Script], hash: Seq[Byte]): EDKleisli[Unit] = {
      addDowns(hash, downs) >> add(MigrationInfo(name = m.name, hash = hash, dateTime = now))
    }

    m.groups.toList.traverse__(applyGroup).conclude {
      (ums, f) => liftE(finalize(ums.downsToApply, failHash(f)) >> failure(describe(m.name, ums, f)))
    } {
      (ums, _) => liftE(finalize(ums.downsToApply, hash))
    }
  }
}