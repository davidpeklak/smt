import java.sql.ResultSet
import scala.io.Codec

object Util {
  def bytesToHex(bs: Seq[Byte]): String = bs.map("%02x" format _).mkString("")

  def hexToBytes(s: String): Seq[Byte] = s.grouped(2).map(_.foldLeft[Int](0)((i, c) => i * 16 + c.asDigit)).map(_.toByte).toList

  def stringToBytes(s: String): Seq[Byte] = s.getBytes

  def bytesToString(bs: Seq[Byte]): String = new String(bs.toArray)

  def ResultSetIterator(rs: ResultSet): Iterator[ResultSet] = new Iterator[ResultSet] {
    def hasNext: Boolean = !rs.isLast

    def next(): ResultSet = {
      rs.next()
      rs
    }
  }
}