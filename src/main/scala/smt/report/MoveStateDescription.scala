package smt.report

import smt.{UpMoveState, DownMoveState, MoveState}

object MoveStateDescription {
  def describe(nms: MoveState): String = nms match {
    case dms: DownMoveState => {
      dms.crashedDown match {
        case None => "rolled back."
        case Some(s) => "roll-back attempted, but crashed at: " + s.name
      }
    }
    case ums: UpMoveState => {
      ums.crashedUp match {
        case None => "applied."
        case Some(s) => "attempted, but crashed at: " + s.name
      }
    }
  }

  def describe(name: String, nms: MoveState): String = name + ": " + describe(nms)
}