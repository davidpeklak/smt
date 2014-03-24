package smt

import java.io.File
import sbt._
import smt.Util._

object FileMigration {
  def apply(name: String, scripts: Seq[File], tests: Seq[Test]): Migration = {
    def script(dir: File, filename: String): Script = Script(name = dir.getName, content = bytesToString(IO.readBytes(dir / filename)))

    Migration(name = name, Seq(Group(ups = scripts.map(d => script(d, "up.sql")), downs = scripts.map(d => script(d, "down.sql")))), tests = tests)
  }
}