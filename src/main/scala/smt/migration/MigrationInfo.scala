package smt.migration

import java.util.Date
import smt.util.Util
import Util._

case class MigrationInfo(name: String, hash: Seq[Byte], dateTime: Date, user: Option[String], remark: Option[String]) {
  override def toString: String = {
    val seq = Seq(Some(name), Some(bytesToHex(hash)), Some(dateTime.toString), user, remark).flatten
    "MigrationInfo(" + seq.mkString(", ") + ")"
  }
}
