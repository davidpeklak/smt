package smt

import org.scalacheck.{Test, Prop}
import org.scalacheck.Test.{Passed, Parameters}
import org.scalatest.FunSuite

trait PropTesting {
  this: FunSuite =>

  def check(p: Prop) {
    val r = Test.check(Parameters.default, p)

    assert(r.status === Passed)
  }
}
