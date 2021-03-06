package smt

import scalaz.{\/, Scalaz, Monoid}
import Scalaz._
import smt.migration.Script

sealed trait Writing

sealed trait MoveState extends Writing

case class Lock(name: String) extends Writing

case class UpMoveState(
                        appliedUps: List[Script] = Nil,
                        appliedUpsWithDowns: List[Script] = Nil,
                        crashedUp: Option[Script] = None,
                        downsToApply: List[Script] = Nil
                        ) extends MoveState

object UpMoveState {
  implicit val sg: Monoid[UpMoveState] = new Monoid[UpMoveState] {
    def append(f1: UpMoveState, f2: => UpMoveState): UpMoveState = {
      UpMoveState(
        f1.appliedUps ⊹ f2.appliedUps,
        f1.appliedUpsWithDowns ⊹ f2.appliedUpsWithDowns,
        f2.crashedUp orElse f1.crashedUp,
        f1.downsToApply ⊹ f2.downsToApply)
    }

    def zero: UpMoveState = UpMoveState()
  }

  def appliedUp(up: Script): UpMoveState = UpMoveState(appliedUps = List(up))

  def appliedUpsWithDowns(ups: List[Script]): UpMoveState = UpMoveState(appliedUpsWithDowns = ups)

  def crashedUp(up: Script): UpMoveState = UpMoveState(crashedUp = Some(up))

  def downsToApply(downs: List[Script]): UpMoveState = UpMoveState(downsToApply = downs)
}

class UpMoveStateHolder {
  var ums = UpMoveState()
  def add(u: UpMoveState): Unit = {
    ums = ums ⊹ u
  }
}

case class DownMoveState(
                          appliedDowns: List[Script] = Nil,
                          crashedDown: Option[Script] = None
                          ) extends MoveState

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

  def crashedDown(down: Script): DownMoveState = DownMoveState(crashedDown = Some(down))
}

class DownMoveStateHolder {
  var dms = DownMoveState()
  def add(d: DownMoveState): Unit = {
    dms = dms ⊹ d
  }
}

case class NamedMoveStates(actions: List[(String, MoveState)])

object NamedMoveStates {
  implicit val sg: Monoid[NamedMoveStates] = new Monoid[NamedMoveStates] {
    def append(f1: NamedMoveStates, f2: => NamedMoveStates): NamedMoveStates = {
      NamedMoveStates(f1.actions ⊹ f2.actions)
    }

    def zero: NamedMoveStates = NamedMoveStates(Nil)
  }
  
  def namedMoveState(name: String)(ms: MoveState): NamedMoveStates = NamedMoveStates(List((name, ms)))
}

class NamedMoveStatesHolder {
  var nms = NamedMoveStates.sg.zero

  def addDownMoveStateOf(name: String)(f: DownMoveStateHolder => String \/ Unit): String \/ Unit = {
    val dms = new DownMoveStateHolder()
    val r = f(dms)
    nms = nms ⊹ NamedMoveStates(List((name, dms.dms)))
    r
  }

  def addUpMoveStateOf(name: String)(f: UpMoveStateHolder => String \/ Unit): String \/ Unit = {
    val ums = new UpMoveStateHolder()
    val r = f(ums)
    nms = nms ⊹ NamedMoveStates(List((name, ums.ums)))
    r
  }
}