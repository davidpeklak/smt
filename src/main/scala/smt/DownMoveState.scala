package smt

import scalaz.{Scalaz, Monoid}
import Scalaz._

case class DownMoveState(
                        appliedDowns: List[Script] = Nil,
                        crashedDown: Option[Script] = None
                        )

object DownMoveState {
  implicit val sg: Monoid[DownMoveState] = new Monoid[DownMoveState] {
    def append(f1: DownMoveState, f2: => DownMoveState): DownMoveState = {
      DownMoveState(
        f1.appliedDowns ⊹ f2.appliedDowns,
        f2.crashedDown orElse f1.crashedDown)
    }

    def zero: DownMoveState = DownMoveState()
  }

  def appliedDown(down: Script): DownMoveState = DownMoveState(appliedDowns = List(down))

  def crashedDown(up: Script): DownMoveState = DownMoveState(crashedDown = Some(up))
}