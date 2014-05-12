package smt.migration

import java.io.File
import smt.db.Connection
import ScriptParsers._
import scalaz.\/

case class Test (run: Connection => String \/ Unit)

object ScriptTest {
  def scriptTest(script: Script): Test = Test(db => db.testScript(script))

  def apply(file: File): Seq[Test] = OneFileOneScriptParser(file).map(scriptTest)

  def apply(sep: String, file: File): Seq[Test] = OneFileManyScriptsParser(sep)(file).map(scriptTest)

  def apply(files: Seq[File]): Seq[Test] = files.flatMap(OneFileOneScriptParser).map(scriptTest)

  def apply(sep: String, files: Seq[File]): Seq[Test] = files.flatMap(OneFileManyScriptsParser(sep)).map(scriptTest)
}



