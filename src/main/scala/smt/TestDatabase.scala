package smt

import Util._

class TestDatabase extends Database {

    def add(migrationInfo: MigrationInfo): Either[String, TestDatabase]  = {
      println("adding " + migrationInfo)
      s =  s :+ migrationInfo
      Right(this)
    }

    def addDowns(migHash: Seq[Byte], downs: Seq[Script]): Either[String, TestDatabase] = {
      println("adding " + downs)
      ds = ds + (migHash -> downs)
      Right(this)
    }

    def remove(hash: Seq[Byte]): Either[String, TestDatabase] = {
      println("removing " + bytesToHex(hash))
      s = s.filterNot(_.hash == hash)
      Right(this)
    }

    def removeDowns(migHash: Seq[Byte]): Either[String, TestDatabase] = {
      println("removing downs" + bytesToHex(migHash))
      ds = ds - migHash
      Right(this)
    }

    def apply(script: Script): Either[String, TestDatabase] = {
      println("applying " + script)
      Right(this)
    }


  private var s: Seq[MigrationInfo] = Nil

  private var ds: Map[Seq[Byte], Seq[Script]] = Map()

  def state: Seq[MigrationInfo] = s

  def downs(hash: Seq[Byte]): Seq[Script] = ds(hash)
}