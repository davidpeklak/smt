package smt

import sbt.Keys._
import java.security.MessageDigest
import smt.util.Util
import Util._
import smt.migration.{Script, Migration, Group}

object MigrationHandling {
  type Transformation = String => String

  def transformGroup(downTs: Seq[Transformation], upTs: Seq[Transformation])(group: Group): Group = {
    group.copy(ups = transformScripts(upTs)(group.ups), downs = transformScripts(downTs)(group.downs))
  }

  def transformScripts(ts: Seq[Transformation])(ss: Seq[Script]): Seq[Script] = ss.map(s => {
    ts.foldLeft(s)((s, t) => s.copy(content = t(s.content)))
  })

  def transformedMigrationsImpl(ms: Seq[Migration], downTs: Seq[Transformation], upTs: Seq[Transformation]): Seq[Migration] = {
    ms.map(m => m.copy(groups =  m.groups.map(transformGroup(downTs, upTs))))
  }

  def showHashesImpl(ms: Seq[Migration], s: TaskStreams): Unit = {
    (ms.map(_.name) zip hashMigrations(ms).map(bytesToHex)).foreach(t => s.log.info(t._1 + ": " + t._2))
  }

  private lazy val md = MessageDigest.getInstance("SHA")

  def hashBytes(bs: Seq[Byte]): Seq[Byte] = synchronized(md.digest(bs.toArray).toSeq)

  def hashMigration(m: Migration, preOpt: Option[Seq[Byte]]): Seq[Byte] = {
    hashBytes(preOpt.getOrElse(Seq()) ++ m.groups.flatMap(_.ups).map(_.content).foldRight(Seq[Byte]())(stringToBytes(_) ++ _))
  }

  def hashMigrations(ms: Seq[Migration]): Seq[Seq[Byte]] = ms.foldLeft[Seq[Seq[Byte]]](Nil)((s, m) => s :+ hashMigration(m, s.lastOption))

  def failHash(failure: String): Seq[Byte] = hashBytes(stringToBytes(failure))

}