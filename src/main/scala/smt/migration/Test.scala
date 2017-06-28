package smt.migration

import java.io.File
import smt.db.Connection
import FileSplitters._
import scalaz.\/
import smt.util.Logger

case class Test (run: Connection => Logger => String \/ Unit)

object ScriptTest {
  def scriptTest(script: Script): Test = Test(db => logger => db.testScript(logger)(script))

  def apply(file: File): Seq[Test] = OneFileOneScriptSplitter(file).map(scriptTest)

  def apply(sep: String, file: File): Seq[Test] = OneFileManyScriptsSplitter(sep)(file).map(scriptTest)

  def apply(files: Seq[File]): Seq[Test] = files.flatMap(OneFileOneScriptSplitter).map(scriptTest)

  def apply(sep: String, files: Seq[File]): Seq[Test] = files.flatMap(OneFileManyScriptsSplitter(sep)).map(scriptTest)
}



