package smt

import java.io.File
import ScriptParsers._

trait Test {
  def run: Database => Either[String, Unit]
}

object ScriptTest {
  private def scriptTest(script: Script): Test = new Test {

    val run: Database => Either[String, Unit] = db => db.testScript(script)._1.toLeft(())
  }

  def apply(file: File): Seq[Test] = OneFileOneScriptParser(file).map(scriptTest)

  def apply(sep: String, file: File): Seq[Test] = OneFileManyScriptsParser(sep)(file).map(scriptTest)

  def apply(files: Seq[File]): Seq[Test] = files.flatMap(OneFileOneScriptParser).map(scriptTest)

  def apply(sep: String, files: Seq[File]): Seq[Test] = files.flatMap(OneFileManyScriptsParser(sep)).map(scriptTest)
}



