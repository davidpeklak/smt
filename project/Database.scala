
trait Transaction {

  type T <: Transaction

  type DB <: Database

  def add(migrationInfo: MigrationInfo): Either[String, T]

  def addDowns(migHash: Seq[Byte], downs: Seq[String]): Either[String, T]

  def remove(hash: Seq[Byte]): Either[String, T]

  def removeDowns(migHash: Seq[Byte]): Either[String, T]

  def apply(script: String): Either[String, T]

  def commit: DB
}

trait Database {

  type T <: Transaction

  def state: Seq[MigrationInfo]

  def downs(hash: Seq[Byte]): Seq[String]

  def transaction: T
}