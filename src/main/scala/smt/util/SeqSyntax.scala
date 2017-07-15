package smt.util

import scala.annotation.tailrec
import scalaz.{\/, -\/, \/-}
import scalaz.syntax.Ops


object SeqSyntax {

  implicit class SeqOps[B](val self: Seq[B]) extends Ops[Seq[B]] {
    def travE[C](f: B => String \/ C): String \/ List[C] = {
      @tailrec
      def go(l: List[B], acc: List[C]): String \/ List[C] = l match {
        case h :: t => f(h) match {
          case -\/(err) => -\/(err)
          case \/-(c) => go(t, c :: acc)
        }
        case Nil => \/-(acc)
      }

      go(self.toList, Nil).map(_.reverse)
    }

    def travE_(f: B => String \/ Unit): String \/ Unit = {
      @tailrec
      def go(l: List[B]): String \/ Unit = l match {
        case h :: t => f(h) match {
          case -\/(err) => -\/(err)
          case \/-(()) => go(t)
        }
        case Nil => \/-()
      }

      go(self.toList)
    }
  }

}
