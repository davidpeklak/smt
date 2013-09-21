package smt

case class Migration ( name: String, ups: Seq[Script], downs: Seq[Script])