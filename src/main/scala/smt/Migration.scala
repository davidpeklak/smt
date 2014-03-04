package smt

case class Group(ups: Seq[Script], downs: Seq[Script])

case class Migration ( name: String, groups: Seq[Group])