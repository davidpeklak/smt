import java.io.File
import sbt._

object FileMigration {
  def apply(name: String, scripts: Seq[File]): Migration =
    Migration(name = name, ups = scripts.map(s => IO.readBytes(s / "up.sql")), downs = scripts.map(s => IO.readBytes(s / "down.sql")))
}