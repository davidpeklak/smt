package smt

import sbt._
import sbt.Keys._
import java.util.Date
import smt.Util._
import scalaz._
import scalaz.syntax.std.option._
import scalaz.\/-
import smt.DbAction.SE

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

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], arb: Boolean, s: TaskStreams): Unit = {
    val action = applyMigrationsImplAction(ms, arb)
    val result = run(action)(db)
    result.swap.foreach(failException(s))
  }
}

object DBHandling {

  import MigrationHandling._

  import FreeDbAction._

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
    state.map(s => latestCommon2(s, mhs))
  }

  def applyMigrationsImplAction(ms: Seq[Migration], arb: Boolean): EFreeDbAction[Unit] = {
    val mhs = ms zip hashMigrations(ms)

    for (lcho <- latestCommon(mhs).map(_.map(_.db.hash));
         _ <- revertToLatestCommon(lcho, arb);
         _ <- applyMigrations(mhs, lcho))
    yield ()
  }

  private case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(latestCommon: Option[Seq[Byte]], arb: Boolean): EFreeDbAction[Unit] = {
    for {
      mis <- migrationsToRevert(latestCommon)
      mids <- sequence(mis.map(enrichMigrationWithDowns))
      _ <- {
        if (mids.isEmpty || arb) sequence(mids.map(revertMigration))
        else failure("Will not roll back migrations " + mids.map(_.mi.name).mkString(", ") + ", because allow-rollback is set to false")
      }
    } yield ()

  }

  private def migrationsToRevert(latestCommon: Option[Seq[Byte]]): EFreeDbAction[Seq[MigrationInfo]] = {
    state.map(_.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash)))
  }

  private def enrichMigrationWithDowns(mi: MigrationInfo): EFreeDbAction[MigrationInfoWithDowns] = {
    downs(mi.hash).map(ds => MigrationInfoWithDowns(mi, ds))
  }

  private def revertMigration(mid: MigrationInfoWithDowns): EFreeDbAction[Unit] = {
    for {
      _ <- applyScripts(mid.downs.reverse, Down)
      _ <- removeDowns(mid.mi.hash)
      _ <- remove(mid.mi.hash)
    } yield ()
  }

  private def applyScripts(ss: Seq[Script], direction: Direction): EFreeDbAction[Unit] = {
    for {
      _ <- sequence(ss.map(s => applyScript(s, direction)))
    } yield ()
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): EFreeDbAction[Unit] = {
    for {
      _ <- sequence(migrationsToApply(mhs, latestCommon).map {
        case (m, h) => applyMigration(m, h)
      })
    } yield ()
  }

  private def migrationsToApply(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[(Migration, Seq[Byte])] = {
    mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
  }

  private def applyGroup(g: Group): EWFreeDbAction[Unit] = {
    val ups = g.ups
    val downs = g.downs

    val go: EFreeDbAction[Unit] =
      for (_ <- sequence(ups.map(up => applyScript(up, Up)))) yield ()

    EWFreeDbAction(WriterT.putWith[FreeDbAction, List[Script], SE[Unit]](go.run)(_.fold(f => Nil, _ => downs.toList)))
  }

  private def applyMigration(m: Migration, hash: Seq[Byte]): EFreeDbAction[Unit] = {

    def finalize(downs: List[Script], hash: Seq[Byte]): EFreeDbAction[Unit] =
      for {
        _ <- addDowns(hash, downs)
        _ <- add(MigrationInfo(name = m.name, hash = hash, dateTime = now))
      } yield ()


    val go: FreeDbAction[SE[Unit]] =
      for {
        gs <- wSequence(m.groups.map(applyGroup)).run.run
        r <- gs match {
          case (downs, -\/(f)) => {
            for {
              _ <- finalize(downs, failHash(f))
              _ <- failure(f)
            } yield ()
          }.run
          case (downs, \/-(_)) => finalize(downs, hash).run
        }
      } yield r

    EFreeDbAction(go)
  }
}