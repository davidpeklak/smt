sbtPlugin := true

name := "smt"

organization := "com.github.davidpeklak"

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172"
)