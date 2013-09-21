package smt

import Util._

class TestDatabase extends Database {
  db =>

  type T = TestTransaction

  class TestTransaction extends Transaction {
    type T  = TestTransaction
    type DB = TestDatabase

    def add(migrationInfo: MigrationInfo): Either[String, TestTransaction]  = {
      println("adding " + migrationInfo)
      s =  s :+ migrationInfo
      Right(this)
    }

    def addDowns(migHash: Seq[Byte], downs: Seq[Script]): Either[String, TestTransaction] = {
      println("adding " + downs)
      db.ds = db.ds + (migHash -> downs)
      Right(this)
    }

    def remove(hash: Seq[Byte]): Either[String, TestTransaction] = {
      println("removing " + bytesToHex(hash))
      s = s.filterNot(_.hash == hash)
      Right(this)
    }

    def removeDowns(migHash: Seq[Byte]): Either[String, TestTransaction] = {
      println("removing downs" + bytesToHex(migHash))
      db.ds = db.ds - migHash
      Right(this)
    }

    def apply(script: Script): Either[String, TestTransaction] = {
      println("applying " + script)
      Right(this)
    }

    def commit: DB = db
  }

  private var s: Seq[MigrationInfo] = Nil

  private var ds: Map[Seq[Byte], Seq[Script]] = Map()

  def state: Seq[MigrationInfo] = s

  def downs(hash: Seq[Byte]): Seq[Script] = ds(hash)

  def transaction: T = new TestTransaction
}