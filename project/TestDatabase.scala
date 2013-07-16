
class TestDatabase extends Database {
  db =>

  type T = TestTransaction

  class TestTransaction extends Transaction {
    type T  = TestTransaction
    type DB = TestDatabase

    def add(migrationInfo: MigrationInfo): Either[String, TestTransaction]  = {
      s =  s :+ migrationInfo
      Right(this)
    }

    def apply(script: String): Either[String, TestTransaction] = {
      println("applying " + script)
      Right(this)
    }

    def addDowns(migName: String, downs: Seq[String]): Either[String, TestTransaction] = {
      db.downs = db.downs + (migName -> downs)
      Right(this)
    }

    def commit: DB = db
  }

  private var s: Seq[MigrationInfo] = Nil

  private var downs: Map[String, Seq[String]] = Map()

  def state: Seq[MigrationInfo] = s

  def transaction: T = new TestTransaction
}