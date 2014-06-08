sbtPlugin := true

organization := "com.github.davidpeklak"

name := "smt"

version := "0.4e-SNAPSHOT"

publishMavenStyle := false

publishTo <<= (version) { version: String =>
   val scalasbt = "http://repo.scala-sbt.org/scalasbt/"
   val (name, url) =
     if (version.contains("-SNAPSHOT")) ("sbt-plugin-snapshots", scalasbt+"sbt-plugin-snapshots")
     else ("sbt-plugin-releases-foo", scalasbt+"sbt-plugin-releases")
   Some(Resolver.url(name, new URL(url))(Resolver.ivyStylePatterns))
}

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.172",
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "org.scalaz" %% "scalaz-concurrent" % "7.0.5",
  "javax.mail" % "mail" % "1.4.7",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalatest" %% "scalatest" % "1.9.2" % "test"
)
