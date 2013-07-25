import sbt._
import sbt.Keys._
import java.io.File

object MyBuild extends Build {

  lazy val proj = Project(id = "SMT",
    base = new File("."),
    settings = Project.defaultSettings ++ SMT.smtSettings
  )

}