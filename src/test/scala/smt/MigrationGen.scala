package smt

import org.scalacheck.Gen
import org.scalacheck.Gen._
import GenUtil._
import smt.db.{Database, DatabaseId}
import smt.migration.{Group, Migration, Script}

object MigrationGen {
  def migGen(databases: Map[DatabaseId, Database]): Gen[Migration] = {
    for {
      dbId <- oneOf(databases.keys.toList)
      name <- nonEmptyAlphaStr
      numberOfGroups <- choose(1, 10)
      groups <- listOfN(numberOfGroups, groupGen)
    } yield Migration(dbId, name, groups, Seq())
  }

  def groupGen: Gen[Group] = {
    for {
      numberOfUpScripts <- choose(1, 10)
      ups <- listOfN(numberOfUpScripts, scriptGen)
      numberOfDownScripts <- choose(1, 10)
      downs <- listOfN(numberOfDownScripts, scriptGen)
    } yield Group(ups, downs)
  }

  // create a string that does not contain the substring "bad"
  def nonBadString[T](length: Int): Gen[String] = {
    listOfN(length, alphaChar).map(_.mkString("")).waitFor(!_.contains("bad"))
  }

  def scriptGen: Gen[Script] = {
    for {
      name <- nonEmptyAlphaStr
      length <- choose(20, 100)
      content <- nonBadString(length)
    } yield Script(name, content)
  }

  def migEq(m1: Migration, m2: Migration): Boolean = {
    def contentSeq(m: Migration): Seq[String] = m.groups.flatMap(_.ups).map(_.content)

    contentSeq(m1) == contentSeq(m2)
  }

  def listOfDistinctMig(databases: Map[DatabaseId, Database])(l: Int): Gen[List[Migration]] = {
    listOfDistinctN(l, migGen(databases), migEq)
  }
}
