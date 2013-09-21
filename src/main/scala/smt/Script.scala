package smt

case class Script(name: String, content: String) {
  override def toString: String = name
}
