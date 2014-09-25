package smt.migration

import sbt._
import scala.annotation.tailrec
import smt.util.Util._

object FileSplitters {

  def OneFileOneScriptSplitter(file: File): Seq[Script] = {
    Seq(Script(name = file.getName, content = bytesToString(IO.readBytes(file))))
  }

  def splitString(sep: String)(content: String): Seq[String] = {
    @tailrec
    def findSepIndices(acc: Seq[Int]): Seq[Int] = {
      val i = content.indexOfSlice(sep, acc.headOption.map(_ + 1).getOrElse(0))
      if (i == -1) acc
      else findSepIndices(i +: acc)
    }

    val sepIs = findSepIndices(Nil).reverse

    if (sepIs.isEmpty) Seq(content)
    else {
      val sections = ((0 - sep.size) +: sepIs :+ content.size).sliding(2).map(s =>(s(0) + sep.size, s(1))).toSeq
      sections.map {
        case (iBegin, iEnd) => content.substring(iBegin, iEnd)
      }
    }
  }

  def OneFileManyScriptsSplitter(sep: String)(file: File): Seq[Script] = {
    val content = bytesToString(IO.readBytes(file))
    val scriptContents = splitString(sep)(content)
    scriptContents match {
      case Seq(oneScript) => Seq(Script(name = file.getName, content = oneScript))
      case _ => {
        scriptContents.zipWithIndex.map {
          case (s, i) => Script(name = file.getName + "_" + i, content = s)
        }
      }
    }
  }
}
