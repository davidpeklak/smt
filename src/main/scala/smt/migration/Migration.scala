package smt.migration

import smt.db.DatabaseId

case class Migration (dbId: DatabaseId, name: String, groups: Seq[Group], tests: Seq[Test])
