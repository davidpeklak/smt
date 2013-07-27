package smt

import java.io.File
import sbt._
import smt.Util._

object FileMigration {
  def apply(name: String, scripts: Seq[File]): Migration =
    Migration(name = name, ups = scripts.map(s => bytesToString(IO.readBytes(s / "up.sql"))), downs = scripts.map(s => bytesToString(IO.readBytes(s / "down.sql"))))
}