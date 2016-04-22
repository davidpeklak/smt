package smt.migration

import sbt._
import FileSplitters._
import DirCrawlers._
import smt.MigrationHandling

object SchemaMigration {
  def apply(name: String, dir: File): Migration = {
    apply(name, Seq(dir), Seq(), ClassicDirCrawler, OneFileOneScriptSplitter, identity, identity)
  }

  def apply(name: String, dirs: Seq[File]): Migration = {
    apply(name, dirs, Seq(), ClassicDirCrawler, OneFileOneScriptSplitter, identity, identity)
  }

  def apply(name: String, dir: File, tests: Seq[Test]): Migration = {
    apply(name, Seq(dir), tests, ClassicDirCrawler, OneFileOneScriptSplitter, identity, identity)
  }

  def apply(name: String, dirs: Seq[File], tests: Seq[Test]): Migration = {
    apply(name, dirs, tests, ClassicDirCrawler, OneFileOneScriptSplitter, identity,identity)
  }

  def apply(name: String,
            dirs: Seq[File],
            tests: Seq[Test],
            dirCrawler: File => Seq[File], // takes a directory and returns a sequence of subdirectories where the files are expected
            fileSplitter: File => Seq[Script], // takes a file and splits it into a sequence of scripts
            downTransformation: String => String, // a specific transformation of down scripts for this migration
            upTransformation: String => String // a specific transformation of up scripts for this migration
             ): Migration = {
    IO.assertDirectories(dirs: _*)

    def upsAndDowns(dir: File): Seq[Group] = {
      val up = dir / "up"
      IO.assertDirectory(up)
      val upFiles = listFilesAlphabetically(up)
      val upFileNames = upFiles.map(_.getName)

      val down = dir / "down"
      IO.assertDirectory(down)
      val downFiles = listFilesAlphabetically(down)
      val downFileNames = downFiles.map(_.getName)

      if (upFileNames != downFileNames) {
        println("ups and downs differ in " + dir.getCanonicalPath)
        println("ups:   " + upFileNames.mkString(", "))
        println("downs: " + downFileNames.mkString(", "))
      }
      assert(upFileNames == downFileNames)

      val upDownFiles = upFiles zip downFiles
      val groups = upDownFiles.map {
        case (upFile, downFile) => Group(ups = fileSplitter(upFile), downs = fileSplitter(downFile).reverse)
      }

      groups.par.map(MigrationHandling.transformGroup(Seq(downTransformation), Seq(upTransformation))).seq
    }

    def listFilesAlphabetically(dir: File): Seq[File] = {
      dir.listFiles.toSeq.sortBy(_.getName)
    }

    val groups =
      for {
        dir <- dirs
        subdir <- dirCrawler(dir)
        group <- upsAndDowns(subdir)
      } yield group

    Migration(name = name, groups = groups, tests = tests)
  }
}

object SepSchemaMigration {
  def apply(sep: String, name: String, dir: File): Migration = {
    SchemaMigration(name, Seq(dir), Seq(), ClassicDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }

  def apply(sep: String, name: String, dirs: Seq[File]): Migration = {
    SchemaMigration(name, dirs, Seq(), ClassicDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }

  def apply(sep: String, name: String, dir: File, tests: Seq[Test]): Migration = {
    SchemaMigration(name, Seq(dir), tests, ClassicDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }

  def apply(sep: String, name: String, dirs: Seq[File], tests: Seq[Test]): Migration = {
    SchemaMigration(name, dirs, tests, ClassicDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }
}

object OneDirSepSchemaMigration {
  def apply(sep: String, name: String, dir: File): Migration = {
    SchemaMigration(name, Seq(dir), Seq(), IdentityDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }

  def apply(sep: String, name: String, dirs: Seq[File]): Migration = {
    SchemaMigration(name, dirs, Seq(), IdentityDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }

  def apply(sep: String, name: String, dir: File, tests: Seq[Test]): Migration = {
    SchemaMigration(name, Seq(dir), tests, IdentityDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }

  def apply(sep: String, name: String, dirs: Seq[File], tests: Seq[Test]): Migration = {
    SchemaMigration(name, dirs, tests, IdentityDirCrawler, OneFileManyScriptsSplitter("\n" + sep), identity, identity)
  }
}
