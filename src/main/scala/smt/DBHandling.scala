package smt

import sbt._
import sbt.Keys._
import java.util.Date
import util.Util._
import UpMoveState._
import DownMoveState._
import NamedMoveStates._
import scalaz._
import scalaz.syntax.std.option._
import scalaz.syntax.std.list._
import scalaz.\/-
import scalaz.Scalaz._
import smt.util.TraverseStackSafeSyntax
import TraverseStackSafeSyntax._
import smt.db.{DbAction, Database}
import smt.migration._
import smt.migration.Group
import scalaz.\/-
import smt.migration.Migration
import scalaz.-\/
import migration.MigrationInfo
import migration.Script

trait DBHandling {

  import MigrationHandling._
  import DbAction._
  import DBHandling._

  private def failException(s: TaskStreams)(e: String) {
    s.log.error(e)
    throw new Exception(e)
  }

  protected def showDbStateImpl(db: Database, s: TaskStreams): Unit = {
    val result = state.run(db).run

    result.foreach(_.foreach(st => s.log.info(st.toString)))
    result.swap.foreach(failException(s))
  }

  protected def showLatestCommonImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val result = latestCommon(ms zip hashMigrations(ms)).run(db).run

    result.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    result.swap.foreach(failException(s))
  }

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, s: TaskStreams): Unit = {
    val action = applyMigrationsImplAction(ms, arb, runTests)
    val result = action.run.run(db).run
    result._2.swap.foreach(failException(s))
  }
}

object DBHandling {

  import MigrationHandling._

  import DbAction._

  val upMoveTypes = writerTypes[UpMoveState]

  val downMoveTypes = writerTypes[DownMoveState]

  val namedMoveTypes = writerTypes[NamedMoveStates]

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

  private def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): EDbKleisli[Option[Common]] = {
    state.map(latestCommon2(_, mhs))
  }

  def applyMigrationsImplAction(ms: Seq[Migration], arb: Boolean, runTests: Boolean): namedMoveTypes.EWDbKleisli[Unit] = {
    import namedMoveTypes._

    val mhs = ms zip hashMigrations(ms)

    for {
      lcho <- liftE(latestCommon(mhs).map(_.map(_.db.hash)))
      _ <- revertToLatestCommon(lcho, arb)
      _ <- applyMigrations(mhs, lcho, runTests)
    } yield ()
  }

  case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean): namedMoveTypes.EWDbKleisli[Unit] = {
    import namedMoveTypes._
    import downMoveTypes.EWSyntax._

    for {
      mis <- liftE(migrationsToRevert(latestCommon))
      mids <- liftE(mis.toList.traverse(enrichMigrationWithDowns))
      _ <- {
        if (mids.isEmpty || arb) mids.toList.traverse[EWDbKleisli, Unit]((mid: MigrationInfoWithDowns) => revertMigration(mid).mapWritten(namedMoveState(mid.mi.name)))
        else liftE(failure("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false"))
      }
    } yield ()

  }

  private def migrationsToRevert(latestCommon: Option[Seq[Byte]]): EDbKleisli[Seq[MigrationInfo]] = {
    state.map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  private def enrichMigrationWithDowns(mi: MigrationInfo): EDbKleisli[MigrationInfoWithDowns] = {
    downs(mi.hash).map(MigrationInfoWithDowns(mi, _))
  }

  def revertMigration(mid: MigrationInfoWithDowns): downMoveTypes.EWDbKleisli[Unit] = {
    import downMoveTypes._
    import EWSyntax._

    def applyScriptAndWrite(down: Script): EWDbKleisli[Unit] = {
      val go = WriterT.putWith(applyScript(down, Down).run) {
        case -\/(_) => crashedDown(down)
        case \/-(_) => appliedDown(down)
      }

      EWDbKleisli(go)
    }

    def rewriteMigration(dms: DownMoveState, hash: Seq[Byte]): EDbKleisli[Unit] = {
      val downsToWrite = mid.downs.reverse.map(Some(_)).zipAll(dms.appliedDowns.map(Some(_)) :+ dms.crashedDown, None, None)
        .dropWhile(t => t._1 == t._2).map(_._1.toSeq).flatten.reverse

      addDowns(hash, downsToWrite) >> add(MigrationInfo(name = mid.mi.name, hash = hash, dateTime = now))
    }

    liftE(removeDowns(mid.mi.hash) >> remove(mid.mi.hash)) >>
      mid.downs.reverse.toList.traverse__(applyScriptAndWrite).recover((dms, f) => {
        liftE(rewriteMigration(dms, failHash(f)) >> failure(describe(mid.mi.name, dms, f)))
      })
  }

  private def applyScripts(ss: Seq[Script], direction: Direction): EDbKleisli[Unit] = {
    ss.toList.traverse__(s => applyScript(s, direction))
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]], runTests: Boolean): namedMoveTypes.EWDbKleisli[Unit] = {
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

  private def testMigration(m: Migration): EDbKleisli[Unit] = {
    m.tests.toList.traverse__(doTest)
  }

  private def migrationsToApply(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[(Migration, Seq[Byte])] = {
    mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
  }

  private def applyGroup(group: Group): upMoveTypes.EWDbKleisli[Unit] = {

    def apl(up: Script): upMoveTypes.EWDbKleisli[Unit] = {
      val go = WriterT.putWith(applyScript(up, Up).run) {
        case -\/(_) => crashedUp(up)
        case \/-(_) => appliedUp(up)
      }

      upMoveTypes.EWDbKleisli(go)
    }

    import upMoveTypes.EWSyntax._

    group.ups.toList.traverse__(apl) :\/-++> (downsToApply(group.downs.toList) ⊹ appliedUpsWithDowns(group.ups.toList))
  }

  def applyMigration(m: Migration, hash: Seq[Byte]): upMoveTypes.EWDbKleisli[Unit] = {
    import upMoveTypes._
    import EWSyntax._

    def finalize(downs: List[Script], hash: Seq[Byte]): EDbKleisli[Unit] = {
      addDowns(hash, downs) >> add(MigrationInfo(name = m.name, hash = hash, dateTime = now))
    }

    m.groups.toList.traverse__(applyGroup).conclude {
      (ums, f) => liftE(finalize(ums.downsToApply, failHash(f)) >> failure(describe(m.name, ums, f)))
    } {
      (ums, _) => liftE(finalize(ums.downsToApply, hash))
    }
  }
}