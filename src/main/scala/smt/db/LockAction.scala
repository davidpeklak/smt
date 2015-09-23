package smt.db

object LockAction {
  type HasLock[α] = α => String
}
