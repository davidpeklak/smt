package smt

trait Test {
  def run: Database => Either[String, Unit]
}
/*
case class ScriptTest(script: Script) extends Test {
  val run: Database => Either[String, Unit] = db => db.applyScript(script)
}
*/


