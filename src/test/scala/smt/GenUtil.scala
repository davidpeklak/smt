package smt

import org.scalacheck.Gen
import scala.annotation.tailrec

object GenUtil {

  def distinctListOfN[T](n: Int, g: Gen[T]): Gen[List[T]] = {
    type GL = Gen[List[T]]

    def oneMore(gl: GL): GL = {
      for (l <- gl;
           t <- g.filter(t => !l.contains(t)))
      yield t +: l
    }

    @tailrec
    def create(i: Int, gl: GL): GL = if (i == 0) gl else create(i - 1, oneMore(gl))

    create(n, Nil)
  }
}
