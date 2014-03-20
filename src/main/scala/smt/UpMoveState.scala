package smt

import scalaz.{Scalaz, Monoid}
import Scalaz._

case class UpMoveState(
                        appliedUps: List[Script] = Nil,
                        crashedUp: Option[Script] = None,
                        downsToApply: List[Script] = Nil
                        )

object UpMoveState {
  implicit val sg: Monoid[UpMoveState] = new Monoid[UpMoveState] {
    def append(f1: UpMoveState, f2: => UpMoveState): UpMoveState = {
      UpMoveState(
        f1.appliedUps ⊹ f2.appliedUps,
        f2.crashedUp orElse f1.crashedUp,
        f1.downsToApply ⊹ f2.downsToApply)
    }

    def zero: UpMoveState = UpMoveState()
  }

  def upApplied(up: Script): UpMoveState = UpMoveState(appliedUps = List(up))
}
