package smt

import sbt.Keys._
import java.security.MessageDigest
import smt.util.Util
import Util._
import smt.migration._

object MigrationHandling {
  type Transformation = String => String

  def transformGroup(downTs: Seq[Transformation], upTs: Seq[Transformation])(group: Group): Group = {
    group.copy(ups = transformScripts(upTs)(group.ups), downs = transformScripts(downTs)(group.downs))
  }

  def transformScripts(ts: Seq[Transformation])(ss: Seq[Script]): Seq[Script] = ss.map(s => {
    ts.foldLeft(s)((s, t) => s.copy(content = t(s.content)))
  })

  def transformedMigrationsImpl(ms: Seq[Migration], downTs: Seq[Transformation], upTs: Seq[Transformation]): Seq[Migration] = {
    ms.map(m => m.copy(groups = m.groups.map(transformGroup(downTs, upTs))))
  }

  def showHashesImpl(ms: Seq[Migration], imo: Option[(Int, String)], s: TaskStreams): Unit = {
    hashMigrations(ms, imo).migs.foreach { case (migration, hash) =>
      import migration._
      s.log.info(s"${dbId.toString}: $name: ${bytesToHex(hash)}")
    }
  }

  private lazy val md = MessageDigest.getInstance("SHA")

  def hashBytes(bs: Seq[Byte]): Seq[Byte] = synchronized(md.digest(bs.toArray).toSeq)

  def hashMigration(m: Migration, preOpt: Option[Seq[Byte]]): Seq[Byte] = {
    hashBytes(preOpt.getOrElse(Seq()) ++ stringToBytes(m.dbId.toString) ++ m.groups.flatMap(_.ups).map(_.content).foldRight(Seq[Byte]())(stringToBytes(_) ++ _))
  }

  def hashMigrations(ms: Seq[Migration], imo: Option[(Int, String)]): HashedMigrationSeq = {
    val hashes = ms.foldLeft[Seq[Seq[Byte]]](Nil)((s, m) => s :+ hashMigration(m, s.lastOption.orElse(imo.map(im => hexToBytes(im._2)))))

    HashedMigrationSeq(
      imo.map(_._1).getOrElse(0),
      ms zip hashes
    )
  }

  def failHash(failure: String): Seq[Byte] = hashBytes(stringToBytes(failure))

  case class Common(db: MigrationInfo, currentName: String) {
    override def toString: String = {
      val seq = Seq(Some(currentName + " (on db: " + db.name), Some(bytesToHex(db.hash)), Some(db.dateTime.toString), db.user, db.remark).flatten
      "CommonMigrationInfo(" + seq.mkString(", ") + ")"
    }
  }

  def latestCommon2(mis: Seq[MigrationInfo], ms: HashedMigrationSeq): Option[Common] = {
    common2(mis, ms).common.lastOption
  }

  case class CommonMigrations(
                               common: Seq[Common],
                               diffOnDb: Seq[MigrationInfo],
                               diffOnRepo: Seq[(Migration, Seq[Byte])]
                             )

  def common2(mis: Seq[MigrationInfo], ms: HashedMigrationSeq): CommonMigrations = {
    val misInit = mis.drop(ms.initMig)

    val (common, different) = (misInit zip ms.migs).span {
      case (mi: MigrationInfo, (_, h)) => mi.hash == h
    }

    CommonMigrations(
      common = common.map {
        case (mi, (m, _)) => Common(mi, m.name)
      },
      diffOnDb = different.map(_._1),
      diffOnRepo = different.map(_._2)
    )
  }


  def migrationsToApply(mhs: HashedMigrationSeq, latestCommon: Option[Seq[Byte]]): HashedMigrationSeq = {
    mhs.copy(migs = mhs.migs.reverse.takeWhile(mh => !latestCommon.exists(_ == mh._2)).reverse)
  }

}