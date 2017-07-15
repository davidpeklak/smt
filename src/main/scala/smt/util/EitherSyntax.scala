package smt.util

import scalaz.syntax.Ops
import scalaz._
import scalaz.-\/
import scalaz.Scalaz._

object EitherSyntax {

  implicit class EitherOps[B](val self: String \/ B) extends Ops[String \/ B] {
    def andFinally(f: => String \/ Unit): String \/ B = {
      (self, f) match {
        case (-\/(a), -\/(ra)) => -\/(a + ra)
        case (-\/(a), \/-(_)) => -\/(a)
        case (\/-(b), -\/(ra)) => -\/(ra)
        case (\/-(b), \/-(_)) => \/-(b)
      }
    }

    def onLeftDo(f: String => String \/ Unit): String \/ B = {
      self match {
        case err@ -\/(e) => f(e) >> err
        case b@ \/-(_) => b
      }
    }
  }

}
