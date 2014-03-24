package smt

import sbt._
import sbt.Keys._
import java.util.Date
import smt.Util._
import smt.DbAction.SE
import UpMoveState._
import scalaz._
import scalaz.syntax.std.option._
import scalaz.syntax.std.list._
import scalaz.\/-
import scalaz.Scalaz._

trait DBHandling {

  import MigrationHandling._
  import FreeDbAction._
  import DBHandling._

  private def failException(s: TaskStreams)(e: String) {
    s.log.error(e)
    throw new Exception(e)
  }

  protected def showDbStateImpl(db: Database, s: TaskStreams): Unit = {
    val result = run(state)(db)

    result.foreach(_.foreach(st => s.log.info(st.toString)))
    result.swap.foreach(failException(s))
  }

  protected def showLatestCommonImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val result = run(latestCommon(ms zip hashMigrations(ms)))(db)

    result.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    result.swap.foreach(failException(s))
  }

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], arb: Boolean, runTests: Boolean, s: TaskStreams): Unit = {
    val action = applyMigrationsImplAction(ms, arb, runTests)
    val result = run(action)(db)
    result.swap.foreach(failException(s))
  }
}

object DBHandling {

  import MigrationHandling._

  import FreeDbAction._

  import EWSyntax._

  def now: Date = new Date

  case class Common(db: MigrationInfo, currentName: String) {
    override def toString: String = {
      "CommonMigrationInfo(" + currentName + " (on db: " + db.name + "), " + bytesToHex(db.hash) + ", " + db.dateTime + ")"
    }
  }

  private def latestCommon2(mis: Seq[MigrationInfo], ms: Seq[(Migration, Seq[Byte])]): Option[Common] = {
    (mis zip ms).takeWhile {
      case (MigrationInfo(_, hi, _), (_, h)) => hi == h
    }.lastOption.map {
      case (mi, (m, _)) => Common(mi, m.name)
    }
  }

  private def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): EFreeDbAction[Option[Common]] = {
    state.map(latestCommon2(_, mhs))
  }

  def applyMigrationsImplAction(ms: Seq[Migration], arb: Boolean, runTests: Boolean): EFreeDbAction[Unit] = {
    val mhs = ms zip hashMigrations(ms)

    for (lcho <- latestCommon(mhs).map(_.map(_.db.hash));
         _ <- revertToLatestCommon(lcho, arb);
         _ <- applyMigrations(mhs, lcho, runTests))
    yield ()
  }

  private case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean): EFreeDbAction[Unit] = {
    for {
      mis <- migrationsToRevert(latestCommon)
      mids <- mis.toList.traverse(enrichMigrationWithDowns)
      _ <- {
        if (mids.isEmpty || arb) mids.toList.traverse(revertMigration)
        else failure("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false")
      }
    } yield ()

  }

  private def migrationsToRevert(latestCommon: Option[Seq[Byte]]): EFreeDbAction[Seq[MigrationInfo]] = {
    state.map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  private def enrichMigrationWithDowns(mi: MigrationInfo): EFreeDbAction[MigrationInfoWithDowns] = {
    downs(mi.hash).map(MigrationInfoWithDowns(mi, _))
  }

  private def revertMigration(mid: MigrationInfoWithDowns): EFreeDbAction[Unit] = {
    applyScripts(mid.downs.reverse, Down) >> removeDowns(mid.mi.hash) >> remove(mid.mi.hash)
  }

  private def applyScripts(ss: Seq[Script], direction: Direction): EFreeDbAction[Unit] = {
    ss.toList.traverse_(s => applyScript(s, direction))
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]], runTests: Boolean): EFreeDbAction[Unit] = {
    migrationsToApply(mhs, latestCommon).toList.traverse_ {
      case (m, h) =>
        if (runTests) applyMigration(m, h) >> testMigration(m)
        else applyMigration(m, h)
    }
  }

  private def testMigration(m: Migration): EFreeDbAction[Unit] = {
    m.tests.toList.traverse_ (doTest)
  }

  private def migrationsToApply(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[(Migration, Seq[Byte])] = {
    mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
  }

  private def applyGroup(group: Group): EWFreeDbAction[Unit] = {
    def apl(up: Script): EWFreeDbAction[Unit] = {
      val go = WriterT.putWith(applyScript(up, Up).run) {
        case -\/(_) => crashedUp(up)
        case \/-(_) => appliedUp(up)
      }

      EWFreeDbAction(go)
    }

    group.ups.toList.traverse_(apl) :\/-++> (downsToApply(group.downs.toList) ⊹ appliedUpsWithDowns(group.ups.toList))
  }

  private def describe(migName: String, ums: UpMoveState, f: String): String = {
    import ums._
    val upsWithoutDowns = appliedUpsWithDowns.map(Some(_)).zipAll(appliedUps.map(Some(_)), None, None)
      .dropWhile(t => t._1 == t._2).map(_._2.toSeq).flatten

    "Failed to fully apply migration " + migName + " \n" +
      "Applied the following up scripts: \n" +
      ums.appliedUps.mkString("\n") + "\n" +
      "The following up script crashed: \n" +
      ums.crashedUp.getOrElse("(None)") + "\n" +
      "Because of: " + f.toString + "\n" +
      "The following up scripts have been applied without recording corresponding down scripts,\n" +
      "revert them manually before continuing:" + "\n" +
      upsWithoutDowns.map(_.name).mkString("\n")
  }

  def applyMigration(m: Migration, hash: Seq[Byte]): EFreeDbAction[Unit] = {

    def finalize(downs: List[Script], hash: Seq[Byte]): EFreeDbAction[Unit] = {
      addDowns(hash, downs) >> add(MigrationInfo(name = m.name, hash = hash, dateTime = now))
    }

    val go: FreeDbAction[SE[Unit]] =
      for {
        gs <- m.groups.toList.traverse_(applyGroup).run.run
        r <- (gs match {
          case (ums, -\/(f)) => finalize(ums.downsToApply, failHash(f)) >> failure(describe(m.name, ums, f))
          case (ums, \/-(_)) => finalize(ums.downsToApply, hash)
        }).run
      } yield r

    EFreeDbAction(go)
  }
}