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
    val lc = latestCommon(db.state, ms zip hashMigrations(ms))(_.hash)(_._2)
    s.log.info(lc.toString)
  }

  private def latestCommon[A, B](as: Seq[A], bs: Seq[B])(f: A => Array[Byte])(g: B => Array[Byte]): Option[(A, B)] = {
    val ahs = as.map(a => (a, f(a)))
    val bhs = bs.map(b => (b, g(b)))
    (ahs zip bhs).takeWhile(ahbh => (ahbh._1._2.toSeq == ahbh._2._2.toSeq)).lastOption.map(ahbh => (ahbh._1._1, ahbh._2._1))
  }

  protected def applyMigrationsImpl(db: Database, ms: Seq[Migration], s: TaskStreams): Unit = {
    val mhs = ms zip hashMigrations(ms)
    mhs.foldLeft[Either[String, Transaction]](Right(db.transaction))((t, mh) => t.right.flatMap(applyMigration(_, mh._1, mh._2)))
  }

  private def applyMigration(t: Transaction, m: Migration, hash: Array[Byte]): Either[String, Transaction] = {
    val mi = MigrationInfo(name = m.name, hash = hash, dateTime = now)
    val te = t.add(mi).right.flatMap(t => t.addDowns(m.name, m.downs))
    m.ups.foldLeft[Either[String, Transaction]](te)((te, up) => te.right.flatMap(_.apply(up)))
  }
}