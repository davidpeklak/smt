import sbt._
import sbt.Keys._
import java.util.Date
import Util._

trait DBHandling {
  this: MigrationHandling =>

  protected def now: Date = new Date

  protected def showDbStateImpl(db: Database, s: TaskStreams): Unit = {
    db.state.foreach(st => s.log.info(st.toString))
  }

  protected def showLatestCommonImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val lc = latestCommon(db.state, ms zip hashMigrations(ms))
    s.log.info(lc.map(bytesToHex).getOrElse("None"))
  }

  private def latestCommon(mis: Seq[MigrationInfo], ms: Seq[(Migration, Seq[Byte])]): Option[Seq[Byte]] = latestCommonGen(mis, ms)(_.hash)(_._2)

  private def latestCommonGen[A, B](as: Seq[A], bs: Seq[B])(f: A => Seq[Byte])(g: B => Seq[Byte]): Option[Seq[Byte]] = {
    val ahs = as.map(a => (a, f(a)))
    val bhs = bs.map(b => (b, g(b)))
    (ahs zip bhs).takeWhile(ahbh => (ahbh._1._2 == ahbh._2._2)).lastOption.map(_._1._2)
  }

  private type DbAction = Transaction => Either[String, Transaction]

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val mhs = ms zip hashMigrations(ms)
    val lc = latestCommon(db.state, mhs)
    val actions = revertToLatestCommon(db, lc) ++ applyMigrations(mhs, lc)
    actions.foldLeft[Either[String, Transaction]](Right(db.transaction))((te, a) =>  te.right.flatMap(a(_)))
  }

  private case class MigrationInfoWithDowns(mi: MigrationInfo, downs: Seq[String])

  private def revertToLatestCommon(db: Database, latestCommon: Option[Seq[Byte]]): Seq[DbAction] = {
    val mis = db.state.reverse.takeWhile(mi => !latestCommon.exists(_ == mi.hash))
    val mids = mis.map(mi => MigrationInfoWithDowns(mi, db.downs(mi.hash)))
    mids.flatMap(revertMigration(_))
  }

  private def revertMigration(mid: MigrationInfoWithDowns): Seq[DbAction] = {
    mid.downs.reverse.map(down => (t: Transaction) => t.apply(down)) :+
      ((t: Transaction) => t.removeDowns(mid.mi.hash)) :+
      ((t: Transaction) => t.remove(mid.mi.hash))
  }

  private def applyMigrations(mhs: Seq[(Migration, Seq[Byte])], latestCommon: Option[Seq[Byte]]): Seq[DbAction] = {
    val amhs = mhs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse
    amhs.flatMap(amh => applyMigration(amh._1, amh._2))
  }

  private def applyMigration(m: Migration, hash: Seq[Byte]): Seq[DbAction] = {
    val mi = MigrationInfo(name = m.name, hash = hash, dateTime = now)
    ((t: Transaction) => t.add(mi)) +:
      ((t: Transaction) => t.addDowns(hash, m.downs)) +:
      m.ups.map(up => (t: Transaction) => t.apply(up))
  }
}