package smt.migration

import java.io.File
import smt.db.DatabaseId
import smt.util.Util._
import smt.util.FileUtil._

object FileMigration {
  def apply(dbId: String, name: String, scripts: Seq[File], tests: Seq[Test]): Migration = {
    def script(dir: File, filename: String): Script = Script(name = dir.getName, content = bytesToString(readBytes(dir / filename)))

    Migration(dbId = DatabaseId(dbId), name = name, Seq(Group(ups = scripts.map(d => script(d, "up.sql")), downs = scripts.map(d => script(d, "down.sql")))), tests = tests)
  }
}