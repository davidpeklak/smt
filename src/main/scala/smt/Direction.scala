package smt

sealed trait Direction

case object Up extends Direction {
  override val toString: String = "up"
}

case object Down extends Direction {
  override val toString: String = "down"
}
