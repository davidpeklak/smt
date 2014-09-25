package smt.migration

import java.io.File
import sbt._
import scala.Some
import scala.Some

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
object DirCrawlers {

  private def checkDirectory(file: File): Option[File] = {
    if (file.exists()) {
      IO.assertDirectory(file)
      Some(file)
    }
    else None
  }
  
  def ClassicDirCrawler(dir: File): Seq[File] = {
    val subdirNames = Seq("table", "function", "package", "procedure", "view", "other")
    
    for {
      subdirname <- subdirNames
      subdir <- checkDirectory(dir / subdirname).toSeq   
    } yield subdir
  }
  
  def IdentityDirCrawler(dir: File): Seq[File] = {
    Seq(dir)
  }
}
