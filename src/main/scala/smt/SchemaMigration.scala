package smt

import sbt._
import ScriptParsers._

/**
 * Assumes the following directory structure:
 * + dir
 * | + table
 * | | + down
 * | | | table1down.sql
 * | | | table2down.sql
 * | | + up
 * | | | table1up.sql
 * | | | table2up.sql
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
 * | + other
 * | | + down
 * | | | other1down.sql
 * | | | other2down.sql
 * | | + up
 * | | | other1up.sql
 * | | | other2up.sql
 */
object SchemaMigration {
  def apply(name: String, dir: File): Migration = {
    apply(name, Seq(dir), OneFileOneScriptParser)
  }

  def apply(name: String, dirs: Seq[File]): Migration = {
    apply(name, dirs, OneFileOneScriptParser)
  }

  def apply(name: String, dirs: Seq[File], scriptParser: File => Seq[Script]): Migration = {
    IO.assertDirectories(dirs: _*)

    def checkDirectory(file: File): Option[File] = {
      if (file.exists()) {
        IO.assertDirectory(file)
        Some(file)
      }
      else None
    }

    def upsAndDowns(dir: File): (Seq[Script], Seq[Script]) = {
      val up = dir / "up"
      IO.assertDirectory(up)
      val upFiles = up.listFiles.toSeq
      val upFileNames = upFiles.map(_.getName)
      val ups = upFiles.flatMap(scriptParser)

      val down = dir / "down"
      IO.assertDirectory(down)
      val downFiles = down.listFiles.toSeq
      val downFileNames = downFiles.map(_.getName)
      val downs = downFiles.flatMap(scriptParser)

      if (upFileNames != downFileNames) {
        println("ups and downs differ in " + dir.getCanonicalPath)
        println("ups:   " + upFileNames.mkString(", "))
        println("downs: " + downFileNames.mkString(", "))
      }
      assert(upFileNames == downFileNames)

      (ups, downs)
    }

    val subdirNames = Seq("table", "function", "package", "procedure", "view", "other")

    val ud =
      for (dir <- dirs;
           subdirname <- subdirNames;
           subdir <- checkDirectory(dir / subdirname).toSeq
      ) yield upsAndDowns(subdir)

    val (upss, downss) = ud.unzip
    val (ups, downs) = (upss.flatten, downss.flatten)

    Migration(name = name, ups = ups, downs = downs)
  }
}

object SepSchemaMigration {
  def apply(sep: String, name: String, dir: File): Migration = {
    SchemaMigration(name, Seq(dir), OneFileManyScriptsParser("\n" + sep))
  }

  def apply(sep: String, name: String, dirs: Seq[File]): Migration = {
    SchemaMigration(name, dirs, OneFileManyScriptsParser("\n" + sep))
  }
}

