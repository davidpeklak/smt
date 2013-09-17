package smt

import java.io.File
import sbt._
import smt.Util._

/**
 * Assumes the following directory structure:
 * + dir
 * | + function
 * | | + down
 * | | | function1down.sql
 * | | | function2down.sql
 * | | + up
 * | | | function1up.sql
 * | | | function2up.sql
 * | + package
 * | | + down
 * | | | package1down.sql
 * | | | package2down.sql
 * | | + up
 * | | | package1up.sql
 * | | | package2up.sql
 * | + procedure
 * | | + down
 * | | | procedure1down.sql
 * | | | procedure2down.sql
 * | | + up
 * | | | procedure1up.sql
 * | | | procedure2up.sql
 * | + view
 * | | + down
 * | | | view1down.sql
 * | | | view2down.sql
 * | | + up
 * | | | view1up.sql
 * | | | view2up.sql
 */
object SchemaMigration {
  def apply(name: String, dir: File): Migration = {
    apply(name, Seq(dir))
  }

  def apply(name: String, dirs: Seq[File]): Migration = {
    IO.assertDirectories(dirs: _*)

    def checkDirectory(file: File): Option[File] = {
      if (file.exists()) {
        IO.assertDirectory(file)
        Some(file)
      }
      else None
    }

    def upsAndDowns(dir: File): Seq[(File, File)] = {
      val up = dir / "up"
      IO.assertDirectory(up)
      val ups = up.listFiles

      val down = dir / "down"
      IO.assertDirectory(down)
      val downs = down.listFiles

      val upNames = ups.map(_.getName).toSeq
      val downNames = downs.map(_.getName).toSeq
      if (upNames != downNames) {
        println("ups and downs differ in " + dir.getCanonicalPath)
        println("ups:   " + upNames.mkString(", "))
        println("downs: " + downNames.mkString(", "))
      }
      assert(upNames == downNames)

      ups zip downs
    }

    val subdirNames = Seq("function", "package", "procedure", "view")

    val ud =
      for (dir <- dirs;
           subdirname <- subdirNames;
           subdir <- checkDirectory(dir / subdirname).toSeq;
           (up, down) <- upsAndDowns(subdir)
      ) yield (up, down)

    val (ups, downs) = ud.unzip

    Migration(name = name, ups = ups.map(up => bytesToString(IO.readBytes(up))), downs = downs.map(down => bytesToString(IO.readBytes(down))))
  }
}
