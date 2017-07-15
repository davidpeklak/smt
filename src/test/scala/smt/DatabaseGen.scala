package smt

import org.scalacheck.Gen
import org.scalacheck.Gen._
import GenUtil._
import smt.db.DatabaseId

object DatabaseGen {

  def dbIdGen(n: Int): Gen[List[DatabaseId]] = {
    listOfDistinctN[DatabaseId](n, nonEmptyAlphaStr.map(DatabaseId), _ == _)
  }

}
