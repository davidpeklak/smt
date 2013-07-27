package smt

case class Migration ( name: String, ups: Seq[String], downs: Seq[String])