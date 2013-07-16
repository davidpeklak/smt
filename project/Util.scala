import scala.io.Codec

object Util {
  def bytesToHex(bs: Seq[Byte]): String = bs.map("%02x" format _).mkString("")

  def stringToBytes(s: String): Seq[Byte] = s.getBytes

  def bytesToString(bs: Seq[Byte]): String = new String(bs.toArray)
}