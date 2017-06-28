package smt.util

import java.io.{FileInputStream, BufferedInputStream, File}

import smt.migration.FileSplitters

object FileUtil {

  implicit class FileOps(file: File) {
    def / (name: String): File = {
       new File(file.getPath + File.pathSeparator + name)
    }
  }

  def readBytes(file: File): Array[Byte] = {
    val fip = new FileInputStream(file)
    try {
      val bis = new BufferedInputStream(fip)
      try {
        Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
      }
      finally {
        bis.close()
      }
    }
    finally {
      fip.close()
    }
  }

  def pathSplit(path: String): Seq[String] = {
     FileSplitters.splitString(File.pathSeparator)(path)
  }
}
