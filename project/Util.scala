object Util {
  def bytesToHex(bs: Array[Byte]): String = bs.map("%02x" format _).mkString("")
}