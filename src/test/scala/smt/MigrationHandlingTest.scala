package smt

import org.scalacheck.Gen
import org.scalacheck.Gen.choose
import org.scalacheck.Prop.forAll
import MigrationGen._
import smt.MigrationHandling._
import org.scalatest.FunSuite
import smt.migration.Migration

class MigrationHandlingTest extends FunSuite with PropTesting {

  def swap[A](as: Seq[A], i1: Int, i2: Int): Seq[A] = {
    as.slice(0, i1) ++: (as(i2) +: as.slice(i1 + 1, i2)) ++: (as(i1) +: as.slice(i2 + 1, as.size))
  }

  test("hashMigrations") {
    // genereate a sequence of migrations. Swap two migrations
    // within the sequence and verify that the hash of the last
    // migration in the sequence has changed

    case class MigSeqAndTwoIndices(ms: Seq[Migration], i1: Int, i2: Int)

    val msa2iGen: Gen[MigSeqAndTwoIndices] = {
      for {
        l <- choose(2, 20)
        ms <- listOfDistinctMig(l)
        i1 <- choose(0, l - 1)
        i2 <- choose(0, l - 1).filter(_ != i1)
      }
      yield MigSeqAndTwoIndices(ms, i1 min i2, i1 max i2)
    }

    val p = forAll(msa2iGen)(msa2i => {
      import msa2i._
      val hash = hashMigrations(ms).last
      val hashSwapped = hashMigrations(swap(ms, i1, i2)).last
      hash != hashSwapped
    })

    check(p)
  }

}
