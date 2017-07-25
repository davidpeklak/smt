package smt

import org.scalacheck.{Prop, Test => ScTest}
import org.scalacheck.Test.{Parameters, Passed}
import org.scalatest.FunSuite

import scalaz.{-\/, \/, \/-}

trait PropTesting {
  this: FunSuite =>

  implicit def eitherProp(e: String \/ Unit): Prop = e match {
    case -\/(_) => Prop.falsified
    case \/-(()) => Prop.proved
  }
}
