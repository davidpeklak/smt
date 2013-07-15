import sbt.Keys._
import java.security.MessageDigest
import Util._

trait MigrationHandling {
  protected def transformedMigrationsImpl(ms: Seq[Migration], ts: Seq[Transformation]): Seq[Migration] = {
    ms.map(m => m.copy(ups = ts.foldLeft(m.ups)((ups, t) => t.transform(ups)), downs = ts.foldLeft(m.downs)((downs, t) => t.transform(downs))))
  }

  protected def showHashesImpl(ms: Seq[Migration], s: TaskStreams): Unit = {
    (ms.map(_.name) zip hashMigrations(ms).map(bytesToHex)).foreach(t => s.log.info(t._1 + ": " + t._2))
  }

  private lazy val md = MessageDigest.getInstance("SHA")

  protected def hashBytes(bs: Array[Byte]): Array[Byte] = md.digest(bs)

  protected def hashMigration(m: Migration, preOpt: Option[Array[Byte]]) = {
    hashBytes(preOpt.getOrElse(Array()) ++ m.ups.foldRight(Array[Byte]())(_ ++ _))
  }

  protected def hashMigrations(ms: Seq[Migration]): Seq[Array[Byte]] = ms.foldLeft[Seq[Array[Byte]]](Nil)((s, m) => s :+ hashMigration(m, s.headOption))
}