package smt

trait Test {
  def run: Database => Either[String, Unit]
}

object ScriptTest {

  def apply(script: Script): Test = new Test {
    val run: Database => Either[String, Unit] = db => db.testScript(script)._1.toLeft(())
  }
}



