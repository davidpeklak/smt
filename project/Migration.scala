import java.io.File
import sbt._

case class Migration ( name: String, ups: Seq[Array[Byte]], downs: Seq[Array[Byte]])