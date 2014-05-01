package smt.migration

case class Migration ( name: String, groups: Seq[Group], tests: Seq[Test])
