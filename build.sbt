sbtPlugin := true

organization := "com.github.davidpeklak"

name := "smt"

version := "0.1.b-SNAPSHOT"

publishMavenStyle := false

publishTo <<= (version) { version: String =>
   val scalasbt = "http://repo.scala-sbt.org/scalasbt/"
   val (name, url) =
     if (version.contains("-SNAPSHOT")) ("sbt-plugin-snapshots", scalasbt+"sbt-plugin-snapshots")
     else ("sbt-plugin-releases", scalasbt+"sbt-plugin-releases")
   Some(Resolver.url(name, new URL(url))(Resolver.ivyStylePatterns))
}

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalatest" %% "scalatest" % "1.9.2" % "test"
)
