package smt

import sbt._
import sbt.Keys._
import java.util.Date
import smt.Util._

trait DBHandling {

  import MigrationHandling._

  import FreeDbAction._

  protected def now: Date = new Date

  private def fail(s: TaskStreams)(e: String) {
    s.log.error(e)
    throw new Exception(e)
  }

  protected def run[A](fdba: FreeDbAction[A], db: Database): Either[String, A] = fdba.foldMap(ffDbkTransition).run(db)

  protected def showDbStateImpl(db: Database, s: TaskStreams): Unit = {
    val result = run(state, db)

    result.right.foreach(_.foreach(st => s.log.info(st.toString)))
    result.left.foreach(fail(s))
  }

  protected def showLatestCommonImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val result = run(latestCommon(ms zip hashMigrations(ms)), db)

    result.right.foreach(lco => s.log.info(lco.map(_.toString).getOrElse("None")))
    result.left.foreach(fail(s))
  }

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

  private def latestCommon(mhs: Seq[(Migration, Seq[Byte])]): FreeDbAction[Option[Common]] = {
    state.map(s => latestCommon2(s, mhs))
  }

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], arb: Boolean, s: TaskStreams): Unit = {
    val mhs = ms zip hashMigrations(ms)

    val action =
      for (lcho <- latestCommon(mhs).map(_.map(_.db.hash));
           _ <- revertToLatestCommon(lcho, arb);
           _ <- applyMigrations(mhs, lcho))
      yield ()

    val result = run(action, db)
    result.left.foreach(fail(s))
  }

  private case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean): FreeDbAction[Unit] = {
    for {
      mis <- migrationsToRevert(latestCommon)
      mids <- sequence(mis.map(enrichMigrationWithDowns))
      _ <- {
        if (mids.isEmpty || arb) sequence(mids.map(revertMigration))
        else failure("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false")
      }
    } yield ()

  }

  private def migrationsToRevert(latestCommon: Option[Seq[Byte]]): FreeDbAction[Seq[MigrationInfo]] = {
    state.map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  private def enrichMigrationWithDowns(mi: MigrationInfo): FreeDbAction[MigrationInfoWithDowns] = {
    downs(mi.hash).map(ds => MigrationInfoWithDowns(mi, ds))
  }

  private def revertMigration(mid: MigrationInfoWithDowns): FreeDbAction[Unit] = {
    for {
      _ <- applyScripts(mid.downs.reverse, Down)
      _ <- removeDowns(mid.mi.hash)
      _ <- remove(mid.mi.hash)
    } yield ()
  }

  private def applyScripts(ss: Seq[Script], direction: Direction): FreeDbAction[Unit] = {
    for {
      _ <- sequence(ss.map(s => applyScript(s, direction)))
    } yield ()
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): FreeDbAction[Unit] = {
    for {
      _ <- sequence(migrationsToApply(mhs, latestCommon).map {
      case (m, h) => applyMigration(m, h)
    })
    } yield ()
  }

  private def migrationsToApply(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[(Migration, Seq[Byte])] = {
    mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
  }

  private def applyGroup(hash: Seq[Byte], g: Group): FreeDbAction[Unit] = {
    for {
      _ <- sequence(g.ups.map(s => applyScript(s, Up)))
      _ <- addDowns(hash, g.downs)
    }
    yield ()
  }

  private def applyMigration(m: Migration, hash: Seq[Byte]): FreeDbAction[Unit] = {
    val mi = MigrationInfo(name = m.name, hash = hash, dateTime = now)
    for {
      _ <- sequence(m.groups.map(applyGroup(hash, _)))
      _ <- add(mi)
    }
    yield ()
  }
}