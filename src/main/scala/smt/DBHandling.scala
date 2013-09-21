package smt

import sbt._
import sbt.Keys._
import java.util.Date
import smt.Util._

trait DBHandling {
  import MigrationHandling._

  protected def now: Date = new Date

  protected def showDbStateImpl(db: Database, s: TaskStreams): Unit = {
    db.state.foreach(st => s.log.info(st.toString))
  }

  protected def showLatestCommonImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val lc = latestCommon(db.state, ms zip hashMigrations(ms))
    s.log.info(lc.map(_.toString).getOrElse("None"))
  }

  case class Common(db: MigrationInfo, currentName: String) {
    override def toString: String = {
      "CommonMigrationInfo(" + currentName + " (on db: " + db.name + "), " + bytesToHex(db.hash) + ", " + db.dateTime + ")"
    }
  }

  private def latestCommon(mis: Seq[MigrationInfo], ms: Seq[(Migration, Seq[Byte])]): Option[Common] = {
    (mis zip ms).takeWhile{case (MigrationInfo(_, hi, _), (_, h)) => hi == h}.lastOption.map{case (mi, (m, _)) => Common(mi, m.name)}
  }

  private def latestCommonGen[A, B](as: Seq[A], bs: Seq[B])(f: A => Seq[Byte])(g: B => Seq[Byte]): Option[Seq[Byte]] = {
    val ahs = as.map(a => (a, f(a)))
    val bhs = bs.map(b => (b, g(b)))
    (ahs zip bhs).takeWhile(ahbh => (ahbh._1._2 == ahbh._2._2)).lastOption.map(_._1._2)
  }

  private type DbAction = Database => Either[String, Database]

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val mhs = ms zip hashMigrations(ms)
    val lc = latestCommon(db.state, mhs).map(_.db.hash)
    val actions = revertToLatestCommon(db, lc) ++ applyMigrations(mhs, lc)
    actions.foldLeft[Either[String, Database]](Right(db))((db, a) =>  db.right.flatMap(a(_))).left.foreach(s => throw new Exception(s))
  }

  private case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[Script])

  private def revertToLatestCommon(db: Database, latestCommon: Option[Seq[Byte]]): Seq[DbAction] = {
    val mis = db.state.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash))
    val mids = mis.map(mi => MigrationInfoWithDowns(mi, db.downs(mi.hash)))
    mids.flatMap(revertMigration)
  }

  private def revertMigration(mid: MigrationInfoWithDowns): Seq[DbAction] = {
    mid.downs.reverse.map(down => (t: Database) => t.apply(down)) :+
      ((t: Database) => t.removeDowns(mid.mi.hash)) :+
      ((t: Database) => t.remove(mid.mi.hash))
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[DbAction] = {
    val amhs = mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
    amhs.flatMap(amh => applyMigration(amh._1, amh._2))
  }

  private def applyMigration(m: Migration, hash: Seq[Byte]): Seq[DbAction] = {
    val mi = MigrationInfo(name = m.name, hash = hash, dateTime = now)
    ((t: Database) => t.add(mi)) +:
      ((t: Database) => t.addDowns(hash, m.downs)) +:
      m.ups.map(up => (t: Database) => t.apply(up))
  }
}