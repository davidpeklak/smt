package smt

import org.scalacheck.{Test => ScTest, Prop}
import org.scalacheck.Test.{Passed, Parameters}
import org.scalatest.FunSuite

trait PropTesting {
  this: FunSuite =>

  def check(p: Prop) {
    val r = ScTest.check(Parameters.default, p)

    assert(r.status === Passed)
  }
}
