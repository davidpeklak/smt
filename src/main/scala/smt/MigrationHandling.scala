package smt

import sbt.Keys._
import java.security.MessageDigest
import Util._

object MigrationHandling {
  type Transformation = String => String

  def transformedMigrationsImpl(ms: Seq[Migration], ts: Seq[Transformation]): Seq[Migration] = {
    def transformScripts(ss: Seq[String]): Seq[String] = ss.map(s => {
      ts.foldLeft(s)((s, t) => t(s))
    })
    ms.map(m => m.copy(ups = transformScripts(m.ups), downs = transformScripts(m.downs)))
  }

  def showHashesImpl(ms: Seq[Migration], s: TaskStreams): Unit = {
    (ms.map(_.name) zip hashMigrations(ms).map(bytesToHex)).foreach(t => s.log.info(t._1 + ": " + t._2))
  }

  private lazy val md = MessageDigest.getInstance("SHA")

  def hashBytes(bs: Seq[Byte]): Seq[Byte] = md.digest(bs.toArray).toSeq

  private def hashMigration(m: Migration, preOpt: Option[Seq[Byte]]) = {
    hashBytes(preOpt.getOrElse(Seq()) ++ m.ups.foldRight(Seq[Byte]())(stringToBytes(_) ++ _))
  }

  def hashMigrations(ms: Seq[Migration]): Seq[Seq[Byte]] = ms.foldLeft[Seq[Seq[Byte]]](Nil)((s, m) => s :+ hashMigration(m, s.lastOption))
}