import java.util.Date
import Util._

case class MigrationInfo(name: String, hash: Seq[Byte], dateTime: Date) {
  override def toString: String = {
    "MigrationInfo(" + name + "," + bytesToHex(hash) + "," + dateTime + ")"
  }
}

trait Transaction {

  type T <: Transaction
  type DB <: Database

  def add(migrationInfo: MigrationInfo): Either[String, T]

  def addDowns(migName: String, downs: Seq[String]): Either[String, T]

  def apply(script: String): Either[String, T]

  def commit: DB
}

trait Database {

  type T <: Transaction

  def state: Seq[MigrationInfo]

  def transaction: T
}