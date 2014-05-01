package smt

import org.scalacheck.Gen
import org.scalacheck.Gen.choose
import org.scalacheck.Prop.forAll
import MigrationGen._
import smt.MigrationHandling._
import GenUtil._
import org.scalatest.FunSuite
import smt.migration.Migration

class MigrationHandlingTest extends FunSuite with PropTesting {

  test("hashMigrations") {
    // genereate a sequence of migrations. Swap two migrations
    // within the sequence and verify that the hash of the last
    // migration in the sequence has changed

    case class MigSeqAndTwoIndices(ms: Seq[Migration], i1: Int, i2: Int)

    val msa2iGen: Gen[MigSeqAndTwoIndices] = {
      for (l <- choose(2, 50);
           ms <- distinctListOfN(l, migGen);
           i1 <- choose(0, l - 1);
           i2 <- choose(0, l - 1).filter(_ != i1))
      yield MigSeqAndTwoIndices(ms, i1 min i2, i1 max i2)
    }

    val p = forAll(msa2iGen)(msa2i => {
      import msa2i._
      val hash = hashMigrations(ms).last
      val swappedMs = ms.slice(0, i1) ++: (ms(i2) +: ms.slice(i1 + 1, i2)) ++: (ms(i1) +: ms.slice(i2 + 1, ms.size))
      val hashSwapped = hashMigrations(swappedMs).last
      hash != hashSwapped
    })

    check(p)
  }

}
