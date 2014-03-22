package smt

import sbt.Keys._
import java.security.MessageDigest
import Util._

object MigrationHandling {
  type Transformation = String => String

  def transformedMigrationsImpl(ms: Seq[Migration], ts: Seq[Transformation]): Seq[Migration] = {
    def transformScripts(ss: Seq[Script]): Seq[Script] = ss.map(s => {
      ts.foldLeft(s)((s, t) => s.copy(content = t(s.content)))
    })
    ms.map(m => m.copy(groups =  m.groups.map( group => group.copy(ups = transformScripts(group.ups), downs = transformScripts(group.downs)))))
  }

  def showHashesImpl(ms: Seq[Migration], s: TaskStreams): Unit = {
    (ms.map(_.name) zip hashMigrations(ms).map(bytesToHex)).foreach(t => s.log.info(t._1 + ": " + t._2))
  }

  private lazy val md = MessageDigest.getInstance("SHA")

  def hashBytes(bs: Seq[Byte]): Seq[Byte] = md.digest(bs.toArray).toSeq

  def hashMigration(m: Migration, preOpt: Option[Seq[Byte]]) = {
    hashBytes(preOpt.getOrElse(Seq()) ++ m.groups.flatMap(_.ups).map(_.content).foldRight(Seq[Byte]())(stringToBytes(_) ++ _))
  }

  def hashMigrations(ms: Seq[Migration]): Seq[Seq[Byte]] = ms.foldLeft[Seq[Seq[Byte]]](Nil)((s, m) => s :+ hashMigration(m, s.lastOption))

  def failHash(failure: String): Seq[Byte] = hashBytes(stringToBytes(failure))

}