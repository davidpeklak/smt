package smt

import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.rng.Seed

import scala.annotation.tailrec

object GenUtil {

  def params(): Parameters = Parameters.default
  def seed(): Seed = Seed.random()

  /* Generates a non-empty string of alpha characters */
  def nonEmptyAlphaStr: Gen[String] = nonEmptyListOf(alphaChar).map(_.mkString)

  def listOfDistinctN[T](n: Int, g: Gen[T], eq: (T, T) => Boolean): Gen[List[T]] = {
    type GL = Gen[List[T]]

    def oneMore(gl: GL): GL = {
      for {
        l <- gl
        t <- g.filter(t => !l.exists(eq(_, t)))
      }
      yield t +: l
    }

    @tailrec
    def create(i: Int, gl: GL): GL = if (i == 0) gl else create(i - 1, oneMore(gl))

    create(n, Nil)
  }
}
