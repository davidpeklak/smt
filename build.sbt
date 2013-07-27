sbtPlugin := true

organization := "com.github.davidpeklak"

name := "smt"

version := "0.1-SNAPSHOT"

publishMavenStyle := false

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172"
)