sbtPlugin := true

organization := "com.github.davidpeklak"

name := "smt"

version := "0.1-SNAPSHOT"

publishMavenStyle := false

publishTo := {
   val scalasbt = "http://repo.scala-sbt.org/scalasbt/"
   val (name, url) =  ("sbt-plugin-snapshots", scalasbt+"sbt-plugin-snapshots")
   Some(Resolver.url(name, new URL(url))(Resolver.ivyStylePatterns))
}

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172"
)

scalaVersion := "2.9.2"
