package smt

import sbt._
import sbt.Keys._
import java.util.Date
import smt.Util._
import DbAction._

trait DBHandling {

  import MigrationHandling._

  protected def now: Date = new Date

  private def fail(s: TaskStreams)(e: String) {
    s.log.error(e)
    throw new Exception(e)
  }

  protected def showDbStateImpl(db: Database, s: TaskStreams): Unit = {
    val result = DbAction.state(db)
    result.right.foreach(_.foreach(st => s.log.info(st.toString)))
    result.left.foreach(fail(s))
  }

  protected def showLatestCommonImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val result = latestCommon(ms zip hashMigrations(ms))(db)

    result.right.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    result.left.foreach(fail(s))
  }

  case class Common(db: MigrationInfo, currentName: String) {
    override def toString: String = {
      "CommonMigrationInfo(" + currentName + " (on db: " + db.name + "), " + bytesToHex(db.hash) + ", " + db.dateTime + ")"
    }
  }

  private def latestCommon(mis: Seq[MigrationInfo], ms: Seq[(Migration, Seq[Byte])]): Option[Common] = {
    (mis zip ms).takeWhile {
      case (MigrationInfo(_, hi, _), (_, h)) => hi == h
    }.lastOption.map {
      case (mi, (m, _)) => Common(mi, m.name)
    }
  }

  private def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): DbAction[Option[Common]] = {
    DbAction.state.map(s => latestCommon(s, mhs))
  }

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val mhs = ms zip hashMigrations(ms)

    val action =
      for (lcho <- latestCommon(mhs).map(_.map(_.db.hash));
           _ <- revertToLatestCommon(lcho);
           _ <- applyMigrations(mhs, lcho))
      yield ()

    val result = action(db)
    result.left.foreach(fail(s))
  }

  private case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(latestCommon: Option[Seq[Byte]]): DbAction[Unit] = {
    for (mis <- migrationsToRevert(latestCommon);
         mids <- sequence(mis.map(enrichMigrationWithDowns));
         _ <- sequence(mids.map(revertMigration)))
    yield ()

  }

  private def migrationsToRevert(latestCommon: Option[Seq[Byte]]): DbAction[Seq[MigrationInfo]] = {
    DbAction.state.map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  private def enrichMigrationWithDowns(mi: MigrationInfo): DbAction[MigrationInfoWithDowns] = {
    downs(mi.hash).map(MigrationInfoWithDowns(mi, _))
  }

  private def revertMigration(mid: MigrationInfoWithDowns): DbAction[Unit] = {
    sequence(Seq(
      applyScripts(mid.downs.reverse),
      removeDowns(mid.mi.hash),
      remove(mid.mi.hash)
    )).map(_ => ())
  }

  private def applyScripts(ss: Seq[Script]): DbAction[Unit] = sequence(ss.map(applyScript)).map(_ => ())

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): DbAction[Unit] = {
    sequence(migrationsToApply(mhs, latestCommon).map {
      case (m, h) => applyMigration(m, h)
    }).map(_ => ())
  }

  private def migrationsToApply(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[(Migration, Seq[Byte])] = {
    mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
  }

  private def applyMigration(m: Migration, hash: Seq[Byte]): DbAction[Unit] = {
    val mi = MigrationInfo(name = m.name, hash = hash, dateTime = now)
    for (_ <- add(mi);
         _ <- addDowns(hash, m.downs);
         _ <- sequence(m.ups.map(applyScript)))
    yield ()
  }
}