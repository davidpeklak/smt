package smt

import org.scalacheck.Gen
import org.scalacheck.Gen._
import smt.migration.{Script, Migration, Group}

object MigrationGen {
  def migGen: Gen[Migration] = {
    for {
      name <- alphaStr
      numberOfGroups <- choose(1, 10)
      groups <- listOfN(numberOfGroups, groupGen)
    } yield Migration(name, groups, Seq())
  }

  def groupGen: Gen[Group] = {
    for {
      numberOfUpScripts <- choose(1, 10)
      ups <- listOfN(numberOfUpScripts, scriptGen)
      numberOfDownScripts <- choose(1, 10)
      downs <- listOfN(numberOfDownScripts, scriptGen)
    } yield Group(ups, downs)
  }

  def scriptGen: Gen[Script] = {
    for {
      name <- alphaStr
      length <- choose(20, 100)
      content <- listOfN(length, alphaChar).map(_.mkString(""))
    } yield Script(name, content)
  }
}
