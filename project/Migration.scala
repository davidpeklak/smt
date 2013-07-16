import java.io.File
import sbt._

case class Migration ( name: String, ups: Seq[String], downs: Seq[String])